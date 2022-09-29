use std::collections::HashMap;

use hyphenation::{Hyphenator, Language, Load, Standard};
use rayon::{
    prelude::{IntoParallelRefIterator, ParallelIterator},
    str::ParallelString,
};

use crate::{
    operation::{
        PdfNumber::{self, *},
        PdfOperation::{self, *},
        PdfString,
    },
    Color, ExternalFont, Font, IndirectFontRef, PageMargins, PdfLayerReference,
};

#[derive(Copy, Clone)]
pub enum TextAlignment {
    Left,
    Right,
    Center,
    Justify,
}

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
    pub fn move_by(&self, pos: (f64, f64)) {
        self.layer.add_operation(MoveTextCursor {
            x: Real(pos.0),
            y: Real(pos.1),
        });
    }
    /// Resets the text position
    pub fn move_to(&self, pos: (f64, f64)) {
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
    /// Set the current font
    pub fn set_font(&self, font: &IndirectFontRef, font_size: f64) {
        self.layer.set_font(font, font_size);
    }
    pub fn set_fill_color(&self, fill_color: Color) {
        self.layer.set_fill_color(fill_color);
    }
    /// Write text at the current text cursor
    pub fn write<S>(&self, text: S)
    where
        S: Into<PdfString>,
    {
        self.layer.add_operation(ShowText { text: text.into() });
    }
    /// Moves cursor to an absolute position in the document and outputs text
    pub fn write_at<S>(&self, text: S, pos: (f64, f64))
    where
        S: Into<PdfString>,
    {
        self.move_to(pos);
        self.layer.add_operation(ShowText { text: text.into() });
    }

    pub fn write_paragraph(
        &self,
        text: &str,
        font: &IndirectFontRef,
        font_size: f64,
        alignment: TextAlignment,
        pos: (f64, f64),
    ) {
        let margins = PageMargins::symmetrical(0.0, pos.0);
        let size = font_size as f32;
        let bytes = match self.layer.get_fonts(font).data {
            Font::BuiltinFont(f) => ExternalFont::from(f).font_bytes,
            Font::ExternalFont(f) => f.font_bytes,
        };
        let page = self.layer.page_dimension();
        // TODO look back to the last Tf and extrapolate the current font/font_size from the string
        let ff = fontdue::Font::from_bytes(bytes, fontdue::FontSettings::default()).unwrap();
        let line_height: f64 = font_size * 1.5;

        let (mut cur_x, mut cur_y): (f64, f64) = pos;

        //Character Dimensions
        let white_space: f64 = ff.rasterize(' ', size).0.advance_width.into();

        let paragraph: Vec<(String, f64)> =
            text.split(' ')
                .enumerate()
                .fold(Vec::new(), |mut line, (i, word)| {
                    if i == 0 {
                        line.push((String::new(), 0.0));
                    }

                    if !word.is_empty() {
                        let word_width = word
                            .par_chars()
                            .map(|char| ff.rasterize(char, size).0.advance_width as f64)
                            .sum::<f64>();
                        if cur_x + word_width < page.width - margins.left {
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
                                        let word_length = word1
                                            .par_chars()
                                            .map(|char| {
                                                ff.rasterize(char, size).0.advance_width as f64
                                            })
                                            .sum::<f64>();
                                        let (str, width) = line.last_mut().unwrap();
                                        str.push_str(&word1);
                                        str.push(' ');
                                        *width += word_length + white_space;
                                        line.push((String::new(), 0.0));
                                        let word = vc[*i..]
                                            .iter()
                                            .filter(|char| !char.is_whitespace())
                                            .collect::<String>();
                                        let word_length = word
                                            .par_chars()
                                            .map(|char| {
                                                ff.rasterize(char, size).0.advance_width as f64
                                            })
                                            .sum::<f64>();
                                        if word_length > 0.0 {
                                            let (str, width) = line.last_mut().unwrap();
                                            str.push_str(&word);
                                            str.push(' ');
                                            *width += word_length + white_space;
                                        }
                                        cur_y -= line_height
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
                        } else {
                            let en_us = Standard::from_embedded(Language::EnglishUS).unwrap();
                            let hyphenated = en_us.hyphenate(word);
                            if !hyphenated.breaks.is_empty() {
                                let hyphen = ff.rasterize('-', size).0.advance_width as f64;
                                let vec_chars: Vec<char> = word.chars().collect();
                                let hyphenated_word = hyphenated
                                    .breaks
                                    .iter()
                                    .rev()
                                    .enumerate()
                                    .find_map(|(i, u)| {
                                        let mut hyphenated_string: String =
                                            vec_chars[..*u].iter().collect();
                                        let chars_width = hyphenated_string
                                            .chars()
                                            .map(|char| {
                                                ff.rasterize(char, size).0.advance_width as f64
                                            })
                                            .sum::<f64>();
                                        if (cur_x + chars_width + hyphen)
                                            >= (page.width - margins.left)
                                        {
                                            match hyphenated.breaks.len() - (i + 1) {
                                                0 => Some((None, word.to_string())),
                                                _ => None,
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
                                match hyphenated_word.0 {
                                    Some(string) => {
                                        let string_width = string
                                            .par_chars()
                                            .map(|char| {
                                                ff.rasterize(char, size).0.advance_width as f64
                                            })
                                            .sum::<f64>();
                                        let (str, width) = line.last_mut().unwrap();
                                        str.push_str(&string);
                                        *width += string_width + white_space;
                                    }
                                    None => (),
                                }
                                cur_y -= line_height;
                                cur_x = margins.left;
                                let remaining = hyphenated_word.1.clone();
                                let rem_width = remaining
                                    .par_chars()
                                    .map(|char| ff.rasterize(char, size).0.advance_width)
                                    .sum::<f32>()
                                    as f64;
                                line.push((String::new(), 0.0));
                                let (str, width) = line.last_mut().unwrap();
                                *width += rem_width + white_space;
                                str.push_str(&remaining);
                                str.push(' ');
                                cur_x += rem_width + white_space;
                            } else {
                                cur_x = margins.left;
                                cur_y -= line_height;
                                line.push((String::new(), 0.0));
                                let (str, width) = line.last_mut().unwrap();
                                *width += word_width + white_space;
                                str.push_str(word);
                                str.push(' ');
                            }
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
                    .for_each(|(i, (line, _))| {
                        self.layer.add_operation(ShowText { text: line.into() });
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
                        startx = (page.width - margins.right) - (line_width);
                    }

                    if let TextAlignment::Center = alignment {
                        startx = page.width / 2.0 - (line_width) / 2.0;
                    }
                    let mut ws = 0.0;
                    if !line.is_empty() {
                        if let Some((str, _)) = just_peek.peek() {
                            if !str.is_empty() {
                                let words_len = line.par_split_whitespace().count();
                                let write_area = page.width - pos.0 * 2.0;
                                ws = (write_area - line_width) / (words_len as f64 - 1.0);
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
                paragraph.into_iter().for_each(|(line, line_width)| {
                    let startx = (page.width - margins.right) - line_width;
                    self.move_to((startx, starty));
                    self.write(line);
                    starty -= line_height;
                });
            }
            TextAlignment::Center => {
                self.layer.set_word_spacing(0.0);
                let mut starty = pos.1;
                paragraph.into_iter().for_each(|(line, line_width)| {
                    let startx = (page.width - line_width) / 2.0;
                    self.write_at(line, (startx, starty));
                    starty -= line_height;
                });
            }
        }
    }
}
