use crate::{PdfLayerReference, operation::{PdfOperation, PdfNumber}};

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
    pub fn rectangle(&self, start_pos: (f32, f32), width: f32, height: f32) {
        self.move_to(start_pos);
        self.line_to((start_pos.0, start_pos.1 + height));
        self.line_to((start_pos.0 + width, start_pos.1 + height));
        self.line_to((start_pos.0 + width, start_pos.1));
        self.line_to(start_pos);
    }
    pub fn stroke(&self) {
        self.layer.add_operation(PdfOperation::Stroke {  })
    }
    pub fn fill(&self) {
        self.layer.add_operation(PdfOperation::FillAndStrokeNonZero {  })
    }
    pub fn line_width(&self, width: f32) {
        self.layer.set_line_width(width)
    }
}
