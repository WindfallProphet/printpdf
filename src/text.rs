use hyphenation::{Hyphenator, Language, Load, Standard};
use rayon::{prelude::ParallelIterator, str::ParallelString};

use crate::{
    operation::{PdfNumber::*, PdfOperation::*, PdfString},
    Color, ExternalFont, Font, IndirectFontRef, PageMargins, PdfLayerReference, Pt, TextMatrix,
};

#[derive(Copy, Clone)]
pub enum TextAlignment {
    Left,
    Right,
    Center,
    Justify,
}

// fn text_parser(input: &str) -> IResult<&str, &str> {
//     match line_ending(input) {
//         Ok(k) => Ok(k),
//         Err(e) => {
//             match e {
//                 nom::Err::Incomplete(_) => todo!(),
//                 nom::Err::Error(_) => todo!(),
//                 nom::Err::Failure(_) => todo!(),
//             }
//         }
//     }
// }
/// Page object in text mode.
pub struct TextMode<'a> {
    layer: &'a PdfLayerReference,
}

impl<'a> TextMode<'a> {
    pub(crate) fn new(layer: &'a PdfLayerReference) -> Self {
        Self { layer }
    }
    /// Closes the current text object and Begins anew
    pub fn reset_text(&self) {
        self.layer.add_operation(EndText {});
        self.layer.add_operation(BeginText {});
    }
    /// The first time this is called: moves the cursor relative to the documnent
    ///
    /// Hereafter: Move the text cursor relative to the currrent text cursor.
    pub fn move_by(&self, pos: (f32, f32)) {
        self.layer.add_operation(MoveTextCursor {
            x: Real(pos.0),
            y: Real(pos.1),
        });
    }
    /// Resets the text position
    pub fn move_to(&self, pos: (f32, f32)) {
        // Determine if Td was called after a BT, if so reset text pos
        let should_reset = self
            .layer
            .view_op()
            .iter()
            .rev()
            .find_map(|op| {
                // find_map stops at first Some
                if op.operator == "BT" {
                    Some(0)
                } else if op.operator == "Td" {
                    Some(1)
                } else {
                    None
                }
            })
            .unwrap();
        if let 1 = should_reset {
            self.reset_text();
        }
        self.move_by(pos);
    }

    pub fn text_out<S: Into<PdfString>>(&self, text: S, matrix: [f32; 6], pos: (f32, f32)) {
        let (x, y) = abs_to_real(matrix, pos.0, pos.1);
        self.write_at(text, (x, y));
    }
    pub fn new_line(&self) {
        self.layer.add_operation(NextLine {})
    }
    /// Set the current font
    pub fn set_font(&self, font: &IndirectFontRef, font_size: f32) {
        self.layer.set_font(font, font_size);
    }
    pub fn set_fill_color(&self, fill_color: Color) {
        self.layer.set_fill_color(fill_color);
    }
    pub fn set_text_matrix(&self, matrix: TextMatrix) {
        self.layer.set_text_matrix(matrix);
    }
    /// Write text at the current text cursor
    pub fn write<S>(&self, text: S)
    where
        S: Into<PdfString>,
    {
        self.layer.add_operation(ShowText { text: text.into() });
    }
    /// Moves cursor to an absolute position in the document and outputs text
    pub fn write_at<S>(&self, text: S, pos: (f32, f32))
    where
        S: Into<PdfString>,
    {
        self.move_to(pos);
        self.layer.add_operation(ShowText { text: text.into() });
    }
    pub fn text_width(&self, text: &str, font: &IndirectFontRef, font_size: f32) -> f32 {
        let bytes = match self.layer.get_fonts(font).data {
            Font::BuiltinFont(f) => ExternalFont::from(f).font_bytes,
            Font::ExternalFont(f) => f.font_bytes,
        };
        let ff = fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default()).unwrap();
        text.par_chars()
            .map(|char| ff.rasterize(char, font_size).0.advance_width)
            .sum::<f32>()
    }
    pub fn write_paragraph<P>(
        &self,
        text: &str,
        font: &IndirectFontRef,
        font_size: f32,
        alignment: TextAlignment,
        pos: (P, P),
    ) where
        P: Into<f32>,
    {
        //let margins = PageMargins::symmetrical(0.0, pos.0);
        let hyphenation = true;
        let bytes = match self.layer.get_fonts(font).data {
            Font::BuiltinFont(f) => ExternalFont::from(f).font_bytes,
            Font::ExternalFont(f) => f.font_bytes,
        };
        let page = self.layer.page_dimension();
        let page_width: f32 = page.width.into();
        let page_height: f32 = page.height.into();
        // TODO look back to the last Tf and extrapolate the current font/font_size from the string
        let ff = fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default()).unwrap();
        let font_size = font_size;
        let line_height: f32 = font_size * 1.5;
        let pos = (pos.0.into(), pos.1.into());
        let (mut cur_x, _) = pos;

        //Character Dimensions
        let white_space: f32 = ff.rasterize(' ', font_size).0.advance_width;
        let hyphen = match hyphenation {
            true => Some(ff.rasterize('-', font_size).0.advance_width),
            false => None,
        };

        let string_width = |word: &str| {
            word.par_chars()
                .map(|char| ff.rasterize(char, font_size).0.advance_width)
                .sum::<f32>()
        };
        let new_line = |vec: &mut Vec<(String, f32)>| vec.push((String::new(), 0.0));
        let paragraph: Vec<(String, f32)> =
            text.split(' ')
                .enumerate()
                .fold(Vec::new(), |mut line, (i, word)| {
                    if i == 0 {
                        new_line(&mut line);
                    }
                    if !word.is_empty() {
                        let word_width = string_width(word);
                        if (cur_x + word_width) < (page_width - pos.0) {
                            if word.contains(char::is_whitespace) {
                                let vc: Vec<char> = word.chars().collect();
                                let ws_chars: Vec<(usize, char)> = word
                                    .par_char_indices()
                                    .filter(|(_, ws)| ws.is_whitespace())
                                    .collect();
                                ws_chars.iter().for_each(|(i, char)| match char {
                                    '\n' | '\r' => {
                                        let (w1, w2) = vc.split_at(*i);
                                        let word1: String = w1.iter().collect();
                                        let word1_length = string_width(&word1);
                                        let (str, width) = line.last_mut().unwrap();
                                        str.push_str(&word1);
                                        str.push(' ');
                                        *width += word1_length + white_space;
                                        new_line(&mut line);
                                        cur_x = pos.0;
                                        let word2 = w2
                                            .iter()
                                            .filter(|char| !char.is_whitespace())
                                            .collect::<String>();
                                        let word2_length = string_width(&word2);
                                        if word2_length > 0.0 {
                                            let (str, width) = line.last_mut().unwrap();
                                            str.push_str(&word2);
                                            str.push(' ');
                                            *width += word2_length + white_space;
                                            cur_x += word2_length + white_space;
                                        }
                                    }
                                    _ => (),
                                });
                            } else {
                                let (str, width) = line.last_mut().unwrap();
                                *width += word_width + white_space;
                                str.push_str(word);
                                str.push(' ');
                                cur_x += word_width + white_space;
                            }
                        } else if hyphenation {
                            let en_us = Standard::from_embedded(Language::EnglishUS).unwrap();
                            let hyphenated = en_us.hyphenate(word);
                            if !hyphenated.breaks.is_empty() {
                                let vec_chars: Vec<char> = word.chars().collect();
                                let hyphenated_word = hyphenated
                                    .breaks
                                    .iter()
                                    .rev()
                                    .enumerate()
                                    .find_map(|(i, u)| {
                                        let mut hyphenated_string: String =
                                            vec_chars[..*u].iter().collect();
                                        let chars_width = string_width(&hyphenated_string);
                                        if (cur_x
                                            + chars_width
                                            + hyphen.expect("Hyphenation is disabled"))
                                            > (page_width - pos.0)
                                        {
                                            if hyphenated.breaks.len() - (i + 1) == 0 {
                                                Some((None, word.to_string()))
                                            } else {
                                                None
                                            }
                                        } else {
                                            //Don't add another hyphen if the word is already hyphenated
                                            if !hyphenated_string.ends_with('-') {
                                                hyphenated_string.push('-');
                                            }
                                            let remaining =
                                                vec_chars[*u..].iter().collect::<String>();
                                            Some((Some(hyphenated_string), remaining))
                                        }
                                    })
                                    .unwrap();
                                if let Some(string) = hyphenated_word.0 {
                                    let string_width = string_width(&string);
                                    let (str, width) = line.last_mut().unwrap();
                                    str.push_str(&string);
                                    *width += string_width + hyphen.unwrap();
                                }
                                let remaining = hyphenated_word.1.clone();
                                let rem_width = string_width(&remaining);
                                new_line(&mut line);
                                cur_x = pos.0;
                                let (str, width) = line.last_mut().unwrap();
                                *width += rem_width + white_space;
                                str.push_str(&remaining);
                                str.push(' ');
                                cur_x += rem_width + white_space;
                            } else {
                                cur_x = pos.0;
                                new_line(&mut line);
                                let (str, width) = line.last_mut().unwrap();
                                *width += word_width + white_space;
                                cur_x += word_width + white_space;
                                str.push_str(word);
                                str.push(' ');
                            }
                        } else {
                            cur_x = pos.0;
                            new_line(&mut line);
                            let (str, width) = line.last_mut().unwrap();
                            str.push_str(word);
                            str.push(' ');
                            *width += word_width + white_space;
                            cur_x += word_width + white_space;
                        }
                    }
                    line
                });
        match alignment {
            TextAlignment::Left => {
                let mut starty = pos.1;
                self.reset_text();
                self.layer.add_operation(MoveTextCursor {
                    x: pos.0.into(),
                    y: starty.into(),
                });
                paragraph
                    .into_iter()
                    .enumerate()
                    .for_each(|(_, (line, _))| {
                        self.write(line);
                        self.layer.add_operation(MoveTextCursor {
                            x: 0.0.into(),
                            y: Real(-line_height),
                        });
                    });
            }
            TextAlignment::Justify => {
                self.reset_text();
                self.layer.add_operation(MoveTextCursor {
                    x: pos.0.into(),
                    y: pos.1.into(),
                });
                let mut just_peek = paragraph.into_iter().peekable();
                while let Some((line, line_width)) = just_peek.next() {
                    let mut startx = pos.0;
                    if let TextAlignment::Right = alignment {
                        startx = (page_width - pos.0) - (line_width);
                    }

                    if let TextAlignment::Center = alignment {
                        startx = page_width / 2.0 - (line_width) / 2.0;
                    }
                    let mut ws = 0.0;
                    if !line.is_empty() {
                        if let Some((str, _)) = just_peek.peek() {
                            if !str.is_empty() && !str.contains(|c: char| c == '\r' || c == '\n') {
                                let write_area = page_width - pos.0 * 2.0;
                                let words_len = line.par_split_whitespace().count();
                                ws = (write_area - line_width) / (words_len as f32 - 1.0);
                            }
                        }
                    }
                    self.layer.set_word_spacing(ws);
                    self.layer.add_operation(ShowText { text: line.into() });
                    self.layer.add_operation(MoveTextCursor {
                        x: 0.0.into(),
                        y: Real(-line_height),
                    });
                }
            }
            TextAlignment::Right => {
                let mut starty = pos.1;
                paragraph.into_iter().for_each(|(line, mut line_width)| {
                    if let Some(' ') = line.chars().last() {
                        line_width -= white_space;
                    }
                    let startx = (page_width - pos.0) - line_width;
                    self.move_to((startx, starty));
                    self.write(line);
                    starty -= line_height;
                });
            }
            TextAlignment::Center => {
                self.layer.set_word_spacing(0.0);
                let mut starty = pos.1;
                paragraph.into_iter().for_each(|(line, mut line_width)| {
                    if let Some(' ') = line.chars().last() {
                        line_width -= white_space;
                    }
                    let startx = (page_width - line_width) / 2.0;
                    self.write_at(line, (startx, starty));
                    starty -= line_height;
                });
            }
        }
    }
    #[allow(clippy::too_many_arguments)]
    pub fn write_textbox<P>(
        &self,
        text: &str,
        font: &IndirectFontRef,
        font_size: f32,
        alignment: TextAlignment,
        pos: (P, P),
        total_width: P,
        margins: PageMargins,
        line_height: P,
    ) where
        P: Into<f32>,
    {
        //Todo make write_paragraph rely on write_textbox and get rid of redundant code
        //let margins = PageMargins::symmetrical(0.0, pos.0);
        let hyphenation = true;
        let bytes = match self.layer.get_fonts(font).data {
            Font::BuiltinFont(f) => ExternalFont::from(f).font_bytes,
            Font::ExternalFont(f) => f.font_bytes,
        };
        // TODO look back to the last Tf and extrapolate the current font/font_size from the string
        let ff = fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default()).unwrap();
        let font_size: f32 = font_size.into();
        let lh: f32 = line_height.into();
        let line_height: f32 = font_size * lh;
        let total_width: f32 = total_width.into();
        let pos: (f32, f32) = (pos.0.into(), pos.1.into());

        let mut cur_x = margins.left;
        //Character Dimensions
        let white_space: f32 = ff.rasterize(' ', font_size).0.advance_width;
        let hyphen = match hyphenation {
            true => Some(ff.rasterize('-', font_size).0.advance_width),
            false => None,
        };

        let string_width = |word: &str| {
            word.par_chars()
                .map(|char| ff.rasterize(char, font_size).0.advance_width)
                .sum::<f32>()
        };
        let new_line = |vec: &mut Vec<(String, f32)>| vec.push((String::new(), 0.0));
        let mut cur_y = pos.0;
        let paragraph: Vec<(String, f32)> =
            text.split(' ')
                .enumerate()
                .fold(Vec::new(), |mut line, (i, word)| {
                    if i == 0 {
                        new_line(&mut line);
                    }
                    if word.contains(char::is_whitespace) {
                        println!("{:?}", word);
                        let mut a = 0_usize;
                        let count = word.chars().filter(|char| char.is_whitespace()).count();
                        let mut ws_peek = word
                            .char_indices()
                            .filter(|(_, c)| c.is_whitespace())
                            .peekable();
                        while let Some((i, c)) = ws_peek.next() {
                            if i == 0 { //whitespace is at the beginning of the string
                            }
                            if c == '\r' {
                                if let Some((i, '\n')) = ws_peek.peek() {
                                    let w1: &str = word[a..*i - 1].into();
                                    println!("W1: {:?}", w1);
                                    let w1_width = string_width(w1);
                                    if (w1_width + cur_x) > (total_width - margins.right) {
                                        new_line(&mut line);
                                    }
                                    let (str, width) = line.last_mut().unwrap();
                                    str.push_str(w1);
                                    *width += w1_width;
                                    new_line(&mut line);
                                    cur_x = margins.left;
                                    new_line(&mut line);

                                    let w2: &str = word[*i + 1..].into();
                                    println!("W2: {:?}", w2);

                                    if !w2.contains(|c: char| c == '\r' || c == '\n') {
                                        let (str, width) = line.last_mut().unwrap();
                                        let w2_width = string_width(w2);
                                        str.push_str(w2);
                                        str.push(' ');
                                        *width += w2_width;
                                        cur_x += w2_width + white_space;

                                        break;
                                    } else {
                                        a = *i + 1;
                                        ws_peek.next();
                                    }
                                }
                            } else if c == '\n' {
                                let w1: &str = word[a..i].into();
                                let (str, width) = line.last_mut().unwrap();
                                str.push_str(w1);
                                *width += string_width(w1);
                                new_line(&mut line);
                                cur_x = margins.left;
                                new_line(&mut line);

                                if ws_peek.peek().is_none() {
                                    let w2: &str = word[i + 1..].into();
                                    let (str, width) = line.last_mut().unwrap();
                                    let w2_width = string_width(w2);
                                    str.push_str(w2);
                                    str.push(' ');
                                    *width += w2_width;
                                    cur_x += w2_width + white_space;
                                    break;
                                } else {
                                    a = i + 1;
                                }
                            }
                        }
                    } else if !word.is_empty() {
                        let word_width = string_width(word);
                        if (cur_x + word_width) < (total_width - margins.left) {
                            let (str, width) = line.last_mut().unwrap();
                            *width += word_width + white_space;
                            str.push_str(word);
                            str.push(' ');
                            cur_x += word_width + white_space;
                        } else if hyphenation {
                            let en_us = Standard::from_embedded(Language::EnglishUS).unwrap();
                            let hyphenated = en_us.hyphenate(word);
                            if !hyphenated.breaks.is_empty() {
                                let vec_chars: Vec<char> = word.chars().collect();
                                let hyphenated_word = hyphenated
                                    .breaks
                                    .iter()
                                    .rev()
                                    .enumerate()
                                    .find_map(|(i, u)| {
                                        let mut hyphenated_string: String =
                                            vec_chars[..*u].iter().collect();
                                        let chars_width = string_width(&hyphenated_string);
                                        if (cur_x
                                            + chars_width
                                            + hyphen.expect("Hyphenation is disabled"))
                                            > (total_width - pos.0)
                                        {
                                            if hyphenated.breaks.len() - (i + 1) == 0 {
                                                Some((None, word.to_string()))
                                            } else {
                                                None
                                            }
                                        } else {
                                            //Don't add another hyphen if the word is already hyphenated
                                            if !hyphenated_string.ends_with('-') {
                                                hyphenated_string.push('-');
                                            }
                                            let remaining =
                                                vec_chars[*u..].iter().collect::<String>();
                                            Some((Some(hyphenated_string), remaining))
                                        }
                                    })
                                    .unwrap();
                                if let Some(string) = hyphenated_word.0 {
                                    let string_width = string_width(&string);
                                    let (str, width) = line.last_mut().unwrap();
                                    str.push_str(&string);
                                    *width += string_width + hyphen.unwrap();
                                }
                                let remaining = hyphenated_word.1.clone();
                                let rem_width = string_width(&remaining);
                                new_line(&mut line);
                                cur_x = margins.left;
                                let (str, width) = line.last_mut().unwrap();
                                *width += rem_width + white_space;
                                str.push_str(&remaining);
                                str.push(' ');
                                cur_x += rem_width + white_space;
                            } else {
                                cur_x = margins.left;
                                new_line(&mut line);
                                let (str, width) = line.last_mut().unwrap();
                                *width += word_width + white_space;
                                cur_x += word_width + white_space;
                                str.push_str(word);
                                str.push(' ');
                            }
                        } else {
                            cur_x = margins.left;
                            new_line(&mut line);
                            let (str, width) = line.last_mut().unwrap();
                            str.push_str(word);
                            str.push(' ');
                            *width += word_width + white_space;
                            cur_x += word_width + white_space;
                        }
                    }
                    line
                });
        match alignment {
            TextAlignment::Left => {
                self.reset_text();
                self.layer.add_operation(MoveTextCursor {
                    x: pos.0.into(),
                    y: pos.1.into(),
                });
                paragraph.into_iter().for_each(|(line, _)| {
                    if !line.is_empty() {
                        self.write(line);
                        self.layer.add_operation(MoveTextCursor {
                            x: 0.0.into(),
                            y: Real(-line_height),
                        });
                        cur_y = -line_height;
                    }
                });
            }
            TextAlignment::Justify => {
                self.reset_text();
                self.layer.add_operation(MoveTextCursor {
                    x: pos.0.into(),
                    y: cur_y.into(),
                });
                let mut just_peek = paragraph.into_iter().peekable();
                while let Some((line, line_width)) = just_peek.next() {
                    let mut startx = pos.0;
                    if let TextAlignment::Right = alignment {
                        startx = (total_width - pos.0) - (line_width);
                    }

                    if let TextAlignment::Center = alignment {
                        startx = total_width / 2.0 - (line_width) / 2.0;
                    }
                    let mut ws = 0.0;
                    if !line.is_empty() {
                        if let Some((str, _)) = just_peek.peek() {
                            if !str.is_empty() {
                                let words_len = line.par_split_whitespace().count();
                                let write_area = total_width - pos.0 * 2.0;
                                ws = (write_area - line_width) / (words_len as f32 - 1.0);
                            }
                        }
                    }
                    self.layer.set_word_spacing(ws);
                    self.layer.add_operation(ShowText { text: line.into() });
                    self.layer.add_operation(MoveTextCursor {
                        x: 0.0.into(),
                        y: Real(-line_height),
                    });
                }
            }
            TextAlignment::Right => {
                let mut starty = pos.1;
                paragraph.into_iter().for_each(|(line, mut line_width)| {
                    if let Some(' ') = line.chars().last() {
                        line_width -= white_space;
                    }
                    let startx = total_width - margins.right - line_width;
                    self.move_to((startx, starty));
                    self.write(line);
                    starty -= line_height;
                });
            }
            TextAlignment::Center => {
                self.layer.set_word_spacing(0.0);
                let mut starty = pos.1;
                paragraph.into_iter().for_each(|(line, mut line_width)| {
                    if let Some(' ') = line.chars().last() {
                        line_width -= white_space;
                    }
                    let startx = (total_width - line_width) / 2.0;
                    self.write_at(line, (startx, starty));
                    starty -= line_height;
                });
            }
        }
    }
}

pub fn abs_to_real(matrix: [f32; 6], x_abs: f32, y_abs: f32) -> (f32, f32) {
    let y: f32 = (y_abs - matrix[5] - (x_abs - matrix[4]) * matrix[1] / matrix[0])
        / (matrix[3] - matrix[2] * matrix[1] / matrix[0]);
    let x_rel = (x_abs - matrix[4] - y * matrix[3]) / matrix[0];
    let y_rel = y;
    (x_rel, y_rel)
}
