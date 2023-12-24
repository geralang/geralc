
use std::ops::Range;

use crate::util::strings::StringIdx;


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceRange {
    file_name: StringIdx,
    file_content: StringIdx,
    start_position: usize,
    end_position: usize,
}

impl SourceRange {
    pub fn new(file_name: StringIdx, file_content: StringIdx, start_position: usize, end_position: usize) -> SourceRange {
        SourceRange {
            file_name,
            file_content,
            start_position,
            end_position
        }
    }

    pub fn file_name(&self) -> StringIdx { self.file_name }
    pub fn file_content(&self) -> StringIdx { self.file_content }
    pub fn start_position(&self) -> usize { self.start_position }
    pub fn end_position(&self) -> usize { self.end_position }
}

impl From<Range<&SourceRange>> for SourceRange {
    fn from(value: Range<&SourceRange>) -> SourceRange {
        SourceRange {
            file_name: value.start.file_name,
            file_content: value.start.file_content,
            start_position: value.start.start_position,
            end_position: value.end.end_position
        }
    }
}


pub trait HasSource {
    fn source(&self) -> SourceRange;
}
