extern crate encoding;

pub enum PdfOperation {
    //Objects
    OpenPath {
        x: PdfNumber,
        y: PdfNumber,
    },
    LineTo {
        x: PdfNumber,
        y: PdfNumber,
    },
    Stroke {},
    ClosePath {},
    CloseAndStroke {},
    CloseFillAndStrokeNonZero{},
    CloseFillAndStrokeEvenOdd{},
    FillAndStrokeNonZero{},
    FillAndStrokeEvenOdd{},
    SetStrokeGrayLevel {
        gray: Percentage,
    },
    SetStrokeRGBColor {
        r: Percentage,
        g: Percentage,
        b: Percentage,
    },
    SetLineWidth {
        width: NonNegativePdfNumber,
    },
    RestoreGraphicsState {},


    //Text
    BeginText {},
    EndText {},
    NextLine {},
    MoveTextCursor {
        x: PdfNumber,
        y: PdfNumber,
    },
    SetTextFont {
        font: String,
        size: PdfNumber,
    },
    SetTextLeading {
        leading: PdfNumber,
    },
    ShowText {
        text: PdfString,
    },
    NextLineAndShowText {
        text: PdfString,
    },
    TextRise {
        rise: PdfNumber
    }
    
    // MoveTextSetLeading,
    // SetCharSpacing,
    // SetFontAndSize,
    // SetTextHorizontalScaling,
    // SetTextRenderingmode,
    // SetTextRise,
    // SetWordSpacing,
    // ShowTextAdjusted,
    // ShowTextLine,
    // ShowTextLineAndSpace, // type3 font
    // Type3D0,
    // Type3D1,

    // non stroking color
    // NON_STROKING_COLOR,
    // NON_STROKING_COLOR_N,
    // NON_STROKING_RGB,
    // NON_STROKING_GRAY,
    // NON_STROKING_CMYK,
    // NON_STROKING_COLORSPACE,

    // stroking color
    // STROKING_COLOR,
    // STROKING_COLOR_N,
    // STROKING_COLOR_RGB,
    // STROKING_COLOR_GRAY,
    // STROKING_COLOR_CMYK,
    // STROKING_COLORSPACE,

    // marked content
    // BEGIN_MARKED_CONTENT_SEQ,
    // BEGIN_MARKED_CONTENT,
    // END_MARKED_CONTENT,
    // MARKED_CONTENT_POINT_WITH_PROPS,
    // MARKED_CONTENT_POINT,
    // DRAW_OBJECT,

    // state
    // CONCAT,
    // RESTORE,
    // SAVE,
    // SetFlatness,
    // SetGraphicsStateParams,
    // SetLineCapstyle,
    // SetLineDashpattern,
    // SetLineJoinstyle,
    // SetLineMiterlimit,
    // SetMatrix,
    // SetRenderingintent,

    // // graphics
    // AppendRect,
    // BeginInlineImage,
    // BeginInlineImageData,
    // EndInlineImage,
    // ClipEvenOdd,
    // ClipNonZero,
    // CloseAndStroke,
    // CloseFillEvenOddAndStroke,
    // CloseFillNonZeroAndStroke,
    // //ClosePath,
    // CurveTo,
    // CurveToReplicateFinalPoint,
    // CurveToReplicateInitialPoint,
    // ENDPATH,
    // FillEvenOddAndStroke,
    // FillEvenOdd,
    // FillNonZeroAndStroke
    // FillNonZero,
    // LegacyFillNonZero,
    // LineTo,
    // MoveTo,
    // ShadingFill,
    // StrokePath,

    // text


    // compatibility section
    // BeginCompatibilitySection,
    // EndCompatibilitySection,
}

impl Into<lopdf::content::Operation> for PdfOperation {
    fn into(self) -> lopdf::content::Operation {
        use lopdf::content::Operation;
        macro_rules! operation {
            ($operator:expr $(,)?) => { Operation::new($operator, vec![]) };
            ($operator:expr, $($operants:expr),+ $(,)?) => { Operation::new($operator, vec![$($operants.into()),+]) };
        }
        match self {
            Self::OpenPath { x, y } => operation!("m", x, y),
            Self::ClosePath {} => operation!("h"),
            Self::CloseAndStroke {} => operation!("s"),
            Self::CloseFillAndStrokeNonZero {} => operation!("b"),
            Self::CloseFillAndStrokeEvenOdd {} => operation!("b*"),
            Self::FillAndStrokeNonZero {} => operation!("f"),
            Self::FillAndStrokeEvenOdd {} => operation!("f*") ,
            Self::LineTo { x, y } => operation!("l", x, y),
            Self::Stroke {} => operation!("S"),
            Self::SetStrokeGrayLevel { gray } => operation!("G", gray),
            Self::SetStrokeRGBColor { r, g, b } => operation!("RG", r, g, b),
            Self::SetLineWidth { width } => operation!("w", width),
            Self::BeginText {} => operation!("BT"),
            Self::EndText {} => operation!("ET"),
            Self::NextLine {} => operation!("T*"),
            Self::MoveTextCursor { x, y } => operation!("Td", x, y),
            Self::SetTextFont { font, size } => operation!("Tf", font, size),
            Self::TextRise { rise } => operation!("Ts", rise),
            Self::SetTextLeading { leading } => operation!("TL", leading),
            Self::ShowText { text } => operation!("Tj", text),
            Self::NextLineAndShowText { text } => operation!("'", text),
            Self::RestoreGraphicsState {} => operation!("Q"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PdfNumber {
    Integer(i64),
    Real(f32),
}

impl Into<lopdf::Object> for PdfNumber {
    fn into(self) -> lopdf::Object {
        use lopdf::Object;
        match self {
            Self::Integer(i) => Object::Integer(i),
            Self::Real(r) => Object::Real(r),
        }
    }
}

impl From<i64> for PdfNumber {
    fn from(i: i64) -> Self {
        Self::Integer(i)
    }
}

impl From<f32> for PdfNumber {
    fn from(r: f32) -> Self {
        Self::Real(r)
    }
}

#[derive(Debug, Clone)]
pub enum PdfString {
    Literal(Vec<u8>),
    Hexadecimal(Vec<u8>),
}

impl Into<lopdf::Object> for PdfString {
    fn into(self) -> lopdf::Object {
        use lopdf::{Object, StringFormat};
        match self {
            Self::Literal(s) => Object::String(s, StringFormat::Literal),
            Self::Hexadecimal(s) => Object::String(s, StringFormat::Hexadecimal),
        }
    }
}

impl From<String> for PdfString {
    fn from(s: String) -> Self {
        Self::from(s.as_str())
    }
}

impl<'a> From<&'a str> for PdfString {
    fn from(s: &'a str) -> Self {
        // probably specific to my usecase
        use self::encoding::Encoding as _;
        Self::Literal(
            encoding::all::ISO_8859_1
                .encode(s, encoding::EncoderTrap::Strict)
                .unwrap_or_else(|e| panic!("Encoding failire: {}", e)),
        )
    }
}

#[derive(Debug, Clone, Copy)]
// strictly in range [0..1]
pub struct Percentage(f32);

impl Percentage {
    pub fn new(r: f32) -> Self {
        assert!(r >= 0.);
        assert!(r <= 1.);
        Self(r)
    }
}

impl Into<lopdf::Object> for Percentage {
    fn into(self) -> lopdf::Object {
        lopdf::Object::Real(self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NonNegativePdfNumber {
    Integer(i64),
    Real(f32),
}

impl NonNegativePdfNumber {
    pub fn new_integer(i: i64) -> Self {
        assert!(i >= 0);
        Self::Integer(i)
    }

    pub fn new_real(r: f32) -> Self {
        assert!(r >= 0.);
        Self::Real(r)
    }
}

impl Into<lopdf::Object> for NonNegativePdfNumber {
    fn into(self) -> lopdf::Object {
        use lopdf::Object;
        match self {
            Self::Integer(i) => Object::Integer(i),
            Self::Real(r) => Object::Real(r),
        }
    }
}
