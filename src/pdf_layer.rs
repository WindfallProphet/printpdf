//! PDF layer management. Layers can contain referenced or real content.
use glob_defines::OP_PATH_STATE_SET_LINE_WIDTH;
use hyphenation::{Hyphenator, Language, Load, Standard};
use indices::{PdfLayerIndex, PdfPageIndex};
use lopdf::content::Operation;
use rayon::prelude::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use rayon::str::ParallelString;
use std::rc::Weak;
use std::{cell::RefCell, collections::HashMap};

use crate::operation::{NonNegativePdfNumber, PdfNumber, PdfOperation};
use crate::text::{TextAlignment, TextMode};
use crate::{
    font, BuiltinFont, DirectFontRef, ExternalFont, FontList, PageDimensions, PageMargins,
    PaperSize,
};
use {
    BlendMode, Color, CurTransMat, ExtendedGraphicsStateBuilder, Font, ImageXObject,
    IndirectFontRef, Line, LineCapStyle, LineDashPattern, LineJoinStyle, PdfColor, PdfDocument,
    TextMatrix, TextRenderingMode, XObject, XObjectRef,
};

/// One layer of PDF data
#[derive(Debug, Clone)]
pub struct PdfLayer {
    /// Name of the layer. Must be present for the optional content group
    pub(crate) name: String,
    /// Stream objects in this layer. Usually, one layer == one stream
    pub(super) operations: Vec<Operation>,
}

/// A "reference" to the current layer, allows for inner mutability
/// but only inside this library
#[derive(Debug, Clone)]
pub struct PdfLayerReference {
    /// A weak reference to the document, for inner mutability
    pub document: Weak<RefCell<PdfDocument>>,
    /// The index of the page this layer is on
    pub page: PdfPageIndex,
    /// The index of the layer this layer has (inside the page)
    pub layer: PdfLayerIndex,
    pub margins: PageMargins,
}

impl PdfLayer {
    /// Create a new layer, with a name and what index the layer has in the page
    #[inline]
    pub fn new<S>(name: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            name: name.into(),
            operations: Vec::new(),
        }
    }
}
impl Into<lopdf::Stream> for PdfLayer {
    fn into(self) -> lopdf::Stream {
        use lopdf::{Dictionary, Stream};
        let stream_content = lopdf::content::Content {
            operations: self.operations,
        };

        // page contents may not be compressed (todo: is this valid for XObjects?)
        Stream::new(Dictionary::new(), stream_content.encode().unwrap()).with_compression(false)
    }
}

impl PdfLayerReference {
    /// Add a shape to the layer. Use `closed` to indicate whether the line is a closed line
    /// Use has_fill to determine if the line should be filled.
    pub fn add_shape(&self, line: Line) {
        let line_ops = line.into_stream_op();
        for op in line_ops {
            self.add_operation(op);
        }
    }

    /// Add an image to the layer. To be called from the
    /// `image.add_to_layer()` class (see `use_xobject` documentation)
    pub(crate) fn add_image<T>(&self, image: T) -> XObjectRef
    where
        T: Into<ImageXObject>,
    {
        self.add_xobject(image.into())
    }

    /// Adds a general XObject to the layer, similar to `add_image`,
    /// but allows for other types of XObjects to be added to the
    /// page, not just images
    pub(crate) fn add_xobject<T>(&self, xobject: T) -> XObjectRef
    where
        T: Into<XObject>,
    {
        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let page_mut = &mut doc.pages[self.page.0];

        page_mut.add_xobject(xobject.into())
    }

    /// Begins a new text section
    /// You have to make sure to call `end_text_section` afterwards
    #[inline]
    pub fn begin_text_section(&self) -> () {
        self.add_operation(PdfOperation::BeginText {});
    }

    /// Ends a new text section
    /// Only valid if `begin_text_section` has been called
    #[inline]
    pub fn end_text_section(&self) -> () {
        self.add_operation(PdfOperation::EndText {});
    }

    /// Set the current fill color for the layer
    #[inline]
    pub fn set_fill_color(&self, fill_color: Color) -> () {
        self.add_operation(PdfColor::FillColor(fill_color));
    }

    /// Set the current font, only valid in a `begin_text_section` to
    /// `end_text_section` block
    #[inline]
    pub fn set_font(&self, font: &IndirectFontRef, font_size: f32) -> () {
        self.add_operation(Operation::new(
            "Tf",
            vec![font.name.clone().into(), (font_size).into()],
        ));
    }

    #[inline]
    pub fn line_to(&self, start: (f32, f32), end: (f32, f32)) -> () {
        self.add_operation(PdfOperation::OpenPath { x: PdfNumber::Real(start.0), y: PdfNumber::Real(start.1) },
        );
        self.add_operation(PdfOperation::LineTo { x: PdfNumber::Real(end.0), y: PdfNumber::Real(end.1) },
    );
    self.add_operation(PdfOperation::CloseAndStroke {  })

    }

    /// Set the current line / outline color for the layer
    #[inline]
    pub fn set_outline_color(&self, color: Color) {
        self.add_operation(PdfColor::OutlineColor(color));
    }
    /// Instantiate layers, forms and postscript items on the page
    /// __WARNING__: Object must be added to the same page, since the XObjectRef is just a
    /// String, essentially, it can't be checked that this is the case. The caller is
    /// responsible for ensuring this. However, you can use the `Image` struct
    /// and use `image.add_to(layer)`, which will essentially do the same thing, but ensures
    /// that the image is referenced correctly
    ///
    /// Function is limited to this library to ensure that outside code cannot call it
    pub(crate) fn use_xobject(&self, xobj: XObjectRef, transformations: &[CurTransMat]) {
        // save graphics state
        self.save_graphics_state();

        // do transformations to XObject
        if !transformations.is_empty() {
            let mut t = CurTransMat::Identity;
            let _ = transformations.iter().map(|q| {
                t = CurTransMat::Raw(CurTransMat::combine_matrix(t.into(), (*q).into()));
            });
            self.add_operation(t);
        }

        // invoke object (/Do)
        self.internal_invoke_xobject(xobj.name);

        // restore graphics state
        self.restore_graphics_state();
    }

    /// Set the overprint mode of the stroke color to true (overprint) or false (no overprint)
    pub fn set_overprint_fill(&self, overprint: bool) {
        let new_overprint_state = ExtendedGraphicsStateBuilder::new()
            .with_overprint_fill(overprint)
            .build();

        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let page_mut = &mut doc.pages[self.page.0];

        let new_ref = page_mut.add_graphics_state(new_overprint_state);

        // add gs operator to stream
        page_mut.layers[self.layer.0]
            .operations
            .push(lopdf::content::Operation::new(
                "gs",
                vec![lopdf::Object::Name(new_ref.gs_name.as_bytes().to_vec())],
            ));
    }

    /// Set the overprint mode of the fill color to true (overprint) or false (no overprint)
    /// This changes the graphics state of the current page, don't do it too often or you'll bloat the file size
    pub fn set_overprint_stroke(&self, overprint: bool) {
        // this is technically an operation on the page level
        let new_overprint_state = ExtendedGraphicsStateBuilder::new()
            .with_overprint_stroke(overprint)
            .build();

        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let page_mut = &mut doc.pages[self.page.0];

        let new_ref = page_mut.add_graphics_state(new_overprint_state);
        page_mut.layers[self.layer.0]
            .operations
            .push(lopdf::content::Operation::new(
                "gs",
                vec![lopdf::Object::Name(new_ref.gs_name.as_bytes().to_vec())],
            ));
    }

    /// Set the overprint mode of the fill color to true (overprint) or false (no overprint)
    /// This changes the graphics state of the current page, don't do it too often or you'll bloat the file size
    pub fn set_blend_mode(&self, blend_mode: BlendMode) {
        // this is technically an operation on the page level
        let new_blend_mode_state = ExtendedGraphicsStateBuilder::new()
            .with_blend_mode(blend_mode)
            .build();

        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let page_mut = &mut doc.pages[self.page.0];

        let new_ref = page_mut.add_graphics_state(new_blend_mode_state);

        page_mut.layers[self.layer.0]
            .operations
            .push(lopdf::content::Operation::new(
                "gs",
                vec![lopdf::Object::Name(new_ref.gs_name.as_bytes().to_vec())],
            ));
    }

    /// Set the current line thickness, in points
    ///
    /// __NOTE__: 0.0 is a special value, it does not make the line disappear, but rather
    /// makes it appear 1px wide across all devices
    #[inline]
    pub fn set_line_width(&self, line_width: f32) {
        self.add_operation(PdfOperation::SetLineWidth { width: NonNegativePdfNumber::Real(line_width)},
        );
    }

    /// Set the current line join style for outlines
    #[inline]
    pub fn set_line_join_style(&self, line_join: LineJoinStyle) {
        self.add_operation(line_join);
    }

    /// Set the current line join style for outlines
    #[inline]
    pub fn set_line_cap_style(&self, line_cap: LineCapStyle) {
        self.add_operation(line_cap);
    }

    /// Set the current line join style for outlines
    #[inline]
    pub fn set_line_dash_pattern(&self, dash_pattern: LineDashPattern) {
        self.add_operation(dash_pattern);
    }

    /// Sets (adds to) the current transformation matrix
    /// Use `save_graphics_state()` and `restore_graphics_state()`
    /// to "scope" the transformation matrix to a specific function
    #[inline]
    pub fn set_ctm(&self, ctm: CurTransMat) {
        self.add_operation(ctm);
    }

    /// Sets (replaces) the current text matrix
    /// This does not have to be scoped, since the matrix is replaced
    /// instead of concatenated to the current matrix. However,
    /// you should only call this function with in a block scoped by
    /// `begin_text_section()` and `end_text_section()`
    #[inline]
    pub fn set_text_matrix(&self, tm: TextMatrix) {
        self.add_operation(tm);
    }

    /// Sets the position where the text should appear in Points
    #[inline]
    pub fn set_text_cursor(&self, x: f32, y: f32) {
        self.add_operation(Operation::new(
            "Td",
            vec![lopdf::Object::Real(x), lopdf::Object::Real(y)],
        ));
    }

    /// If called inside a text block scoped by `begin_text_section` and
    /// `end_text_section`, moves the cursor to a new line. PDF does not have
    /// any concept of "alignment" except left-aligned text
    /// __Note:__ Use `set_line_height` earlier to set the line height first
    #[inline]
    pub fn add_line_break(&self) {
        self.add_operation(Operation::new("T*", Vec::new()));
    }

    /// Sets the text line height inside a text block
    /// (must be called within `begin_text_block` and `end_text_block`)
    #[inline]
    pub fn set_line_height(&self, height: f32) {
        self.add_operation(Operation::new("TL", vec![lopdf::Object::Real(height)]));
    }

    /// Sets the character spacing inside a text block
    /// Values are given in points. A value of 3 (pt) will increase
    /// the spacing inside a word by 3pt.
    #[inline]
    pub fn set_character_spacing(&self, spacing: f32) {
        self.add_operation(Operation::new("Tc", vec![lopdf::Object::Real(spacing)]));
    }

    /// Sets the word spacing inside a text block.
    /// Same as `set_character_spacing`, just for words.
    /// __Note:__ This currently does not work for external
    /// fonts. External fonts are encoded with Unicode, and
    /// PDF does not recognize unicode fonts. It only
    /// recognizes builtin fonts done with PDFDoc encoding.
    /// However, the function itself is valid and _will work_
    /// with builtin fonts.
    #[inline]
    pub fn set_word_spacing(&self, spacing: f32) {
        self.add_operation(Operation::new("Tw", vec![lopdf::Object::Real(spacing)]));
    }

    /// Sets the horizontal scaling (like a "condensed" font)
    /// Default value is 100 (regular scaling). Setting it to
    /// 50 will reduce the width of the written text by half,
    /// but stretch the text
    #[inline]
    pub fn set_text_scaling(&self, scaling: f32) {
        self.add_operation(Operation::new("Tz", vec![lopdf::Object::Real(scaling)]));
    }

    /// Offsets the current text positon (used for superscript
    /// and subscript). To reset the superscript / subscript, call this
    /// function with 0 as the offset. For superscript, use a positive
    /// number, for subscript, use a negative number. This does not
    /// change the size of the font
    #[inline]
    pub fn set_line_offset(&self, offset: f32) {
        self.add_operation(Operation::new("Ts", vec![lopdf::Object::Real(offset)]));
    }

    #[inline]
    pub fn set_text_rendering_mode(&self, mode: TextRenderingMode) {
        self.add_operation(Operation::new(
            "Tr",
            vec![lopdf::Object::Integer(mode.into())],
        ));
    }

    /// Add text to the file at the current position by specifying font codepoints for an
    /// ExternalFont
    pub fn write_codepoints<I>(&self, codepoints: I)
    where
        I: IntoIterator<Item = u16>,
    {
        use lopdf::Object::*;
        use lopdf::StringFormat::Hexadecimal;

        let bytes = codepoints
            .into_iter()
            .flat_map(|x| {
                let [b0, b1] = x.to_be_bytes();
                std::iter::once(b0).chain(std::iter::once(b1))
            })
            .collect::<Vec<u8>>();

        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        doc.pages[self.page.0].layers[self.layer.0]
            .operations
            .push(Operation::new("Tj", vec![String(bytes, Hexadecimal)]));
    }

    /// Add text to the file at the current position by specifying
    /// font codepoints with additional kerning offset
    pub fn write_positioned_codepoints<I>(&self, codepoints: I)
    where
        I: IntoIterator<Item = (i64, u16)>,
    {
        use lopdf::Object::*;
        use lopdf::StringFormat::Hexadecimal;

        let list = codepoints.into_iter().map(|(pos, codepoint)| {
                if pos != 0 {
                return Integer(pos)
                }
                let bytes = codepoint.to_be_bytes().to_vec();
                String(bytes, Hexadecimal)
        }).collect();

        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        doc.pages[self.page.0].layers[self.layer.0]
            .operations
            .push(Operation::new("TJ", vec![Array(list)]));
    }
    pub fn page_dimension(&self) -> PageDimensions {
        let doc = self.document.upgrade().unwrap();
        let page_id = self.page.0;
        let page = &doc.borrow().pages;
        let s = &page[page_id];
        PageDimensions {
            width: s.width,
            height: s.height,
        }
    }
    /// Add text to the file at the current position
    ///
    /// If the given font is a built-in font and the given text contains characters that are not
    /// supported by the [Windows-1252][] encoding, these characters will be ignored.
    ///
    /// [Windows-1252]: https://en.wikipedia.org/wiki/Windows-1252
    #[inline]
    pub fn write_text<S>(&self, text: S, font: &IndirectFontRef) -> ()
    where
        S: Into<String>,
    {
        // NOTE: The unwrap() calls in this function are safe, since
        // we've already checked the font for validity when it was added to the document

        use lopdf::Object::*;
        use lopdf::StringFormat::Hexadecimal;

        let text = text.into();

        // we need to transform the characters into glyph ids and then add them to the layer
        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();

        // glyph IDs that make up this string

        // kerning for each glyph id. If no kerning is present, will be 0
        // must be the same length as list_gid
        // let mut kerning_data = Vec::<freetype::Vector>::new();

        let bytes: Vec<u8> = {
            if let Font::ExternalFont(face_direct_ref) = doc.fonts.get_font(font).unwrap().data {
                let font = &face_direct_ref.font_data;
                let list_gid: Vec<u16> = text.chars().filter_map(|ch| font.glyph_id(ch)).collect();
                list_gid
                    .iter()
                    .flat_map(|x| vec![(x >> 8) as u8, (x & 255) as u8])
                    .collect::<Vec<u8>>()
            } else {
                // For built-in fonts, we selected the WinAnsiEncoding, see the Into<LoDictionary>
                // implementation for BuiltinFont.
                lopdf::Document::encode_text(Some("WinAnsiEncoding"), &text)
            }
        };

        doc.pages[self.page.0].layers[self.layer.0]
            .operations
            .push(Operation::new("Tj", vec![String(bytes, Hexadecimal)]));
    }
    pub fn get_fonts(&self, font: &IndirectFontRef) -> DirectFontRef {
        let doc = self.document.upgrade().unwrap();
        let x = doc.borrow();
        let fonts = &x.fonts;
        fonts.get_font(font).unwrap()
    }
    /// Saves the current graphic state
    #[inline]
    pub fn save_graphics_state(&self) {
        self.add_operation(Operation::new("q", Vec::new()));
    }

    /// Restores the previous graphic state
    #[inline]
    pub fn restore_graphics_state(&self) {
        self.add_operation(PdfOperation::RestoreGraphicsState {});
    }

    /// Add text to the file, x and y are measure in millimeter from the bottom left corner
    ///
    /// If the given font is a built-in font and the given text contains characters that are not
    /// supported by the [Windows-1252][] encoding, these characters will be ignored.
    ///
    /// [Windows-1252]: https://en.wikipedia.org/wiki/Windows-1252


    /// Add an operation
    ///
    /// This is the low level function used by other function in this struct.
    /// Notice that [Operation](crate::lopdf::content::Operation) is part of the
    /// `lopdf` crate, which is re-exported by this crate.
    pub fn add_operation<T>(&self, op: T) -> ()
    where
        T: Into<Operation>,
    {
        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let layer = &mut doc.pages[self.page.0].layers[self.layer.0];
        layer.operations.push(op.into());
    }
    pub fn view_op(&self) -> Vec<Operation> {
        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let layer = &mut doc.pages[self.page.0].layers[self.layer.0];
        layer.operations.clone()
    }
    /*
        /// Instantiate SVG data
        #[inline]
        pub fn use_svg(&self, width_mm: f64, height_mm: f64,
                       x_mm: f64, y_mm: f64, svg_data_index: SvgIndex)
        {
            let svg_element_ref = {
                let doc = self.document.upgrade().unwrap();
                let doc = doc.borrow_mut();
                let element = doc.contents.get((svg_data_index.0).0).expect("invalid svg reference");
                (*element).clone()
            };

            let doc = self.document.upgrade().unwrap();
            let mut doc = doc.borrow_mut();

            // todo: what about width / height?
            doc.pages.get_mut(self.page.0).unwrap()
                .layers.get_mut(self.layer.0).unwrap()
                    .layer.push(PdfResource::ReferencedResource(svg_data_index.0.clone()));
        }
    */
    pub fn path_mode<Fn>(&self, f: Fn) 
        where Fn: FnOnce(&PathMode) {
        let pm = PathMode::new(self);
            f(&pm)
    }
    pub fn text_mode<Fn>(&self, f: Fn) where Fn: FnOnce(&TextMode) {
        use self::PdfOperation::{BeginText, EndText};
        self.add_operation(BeginText {  });
        let tm = TextMode::new(self);
        f(&tm);
        self.add_operation(EndText {  });
    }

    // internal function to invoke an xobject
    fn internal_invoke_xobject(&self, name: String) {
        let doc = self.document.upgrade().unwrap();
        let mut doc = doc.borrow_mut();
        let page_mut = &mut doc.pages[self.page.0];

        page_mut.layers[self.layer.0]
            .operations
            .push(lopdf::content::Operation::new(
                "Do",
                vec![lopdf::Object::Name(name.as_bytes().to_vec())],
            ));
    }

}
/// Page object in Path mode.
pub struct PathMode <'a> {
    layer: &'a PdfLayerReference,
}

impl <'a > PathMode <'a> {
    pub(crate) fn new(layer: &'a PdfLayerReference) -> Self {
        Self { layer }
    }

    /// Start a new subpath and move the current point for drawing path,
    pub fn move_to(&self, pos: (f32, f32)) -> ()
    {
       self.layer.add_operation(PdfOperation::OpenPath { x: PdfNumber::Real(pos.0), y: PdfNumber::Real(pos.1) })

    }
    pub fn line_to(&self, pos: (f32, f32)) {
        self.layer.add_operation(PdfOperation::LineTo { x: PdfNumber::Real(pos.0), y: PdfNumber::Real(pos.1) })
    }
    pub fn close_path_and_stroke(&self) {
        self.layer.add_operation(PdfOperation::CloseAndStroke {  })
    }
}
