use std::{
    ffi::CStr,
    fmt,
    os::raw::{c_char, c_float, c_long, c_short, c_uchar, c_uint, c_ushort},
    slice, str,
};

#[repr(C)]
pub struct Path {
    _private: [u8; 0],
}

/// # Safety
/// This struct is UB to have a mutable reference to, because if has self-recursion
#[repr(C)]
pub struct Node<'a> {
    // prev: *mut raw_node,
    pub prev: Option<&'a Node<'a>>,

    // next: *mut raw_node,
    pub next: Option<&'a Node<'a>>,

    // enext: *mut raw_node,
    pub enext: Option<&'a Node<'a>>,

    // bnext: *mut raw_node,
    pub bnext: Option<&'a Node<'a>>,

    rpath: *const Path,
    lpath: *const Path,
    // invariant: must be a valid pointer to "some bytes"
    surface: *const c_char,
    feature: *const c_char,
    id: c_uint,
    length: c_ushort,
    rlength: c_ushort,
    rcattr: c_ushort,
    lcattr: c_ushort,
    posid: c_ushort,
    char_type: c_uchar,
    stat: c_uchar,
    isbest: c_uchar,
    alpha: c_float,
    beta: c_float,
    prob: c_float,
    wcost: c_short,
    cost: c_long,
}

impl<'a> Node<'a> {
    /// The visible surface of this node
    /// # Panics
    /// If the `surface` string isn't a valid UTF-8 string.
    pub fn surface(&self) -> &'a str {
        // Safety: Library invariant says that this *must* be a valid slice.
        let bytes = unsafe { slice::from_raw_parts(self.surface as *const u8, self.length.into()) };
        str::from_utf8(bytes).expect("surface was not utf-8")
    }

    /// The surface of this node, including proceeding whitespace
    ///
    /// # Panics
    /// If the `surface` string + the proceeding whitespace isn't a valid UTF-8 string.
    pub fn full_surface(&self) -> &'a str {
        let offset: usize = (self.rlength - self.length).into();
        // Safety: FFI invariant says `rlength - length` gives a prefix, which must be part of the same alloc.
        let ptr = unsafe { self.surface.sub(offset) as *const u8 };

        // Safety: Library invariant says that this *must* be a valid slice.
        // we use rlength here because `-(rlength - length) + rlength = length`
        let bytes = unsafe { slice::from_raw_parts(ptr, self.rlength.into()) };
        str::from_utf8(bytes).expect("surface was not utf-8")
    }

    /// The proceeding whitespace to this node's surface.
    ///
    /// # Panics
    /// If the proceeding whitespace isn't a valid UTF-8 string.
    pub fn whitespace(&self) -> &'a str {
        let offset: usize = (self.rlength - self.length).into();
        // Safety: FFI invariant says `rlength - length` gives a prefix, which must be part of the same alloc.
        let ptr = unsafe { self.surface.sub(offset) as *const u8 };

        // Safety: Library invariant says that this *must* be a valid slice.
        // we use offsest here because we want the string that *ends* where the `surface` begins.
        let bytes = unsafe { slice::from_raw_parts(ptr, offset) };
        str::from_utf8(bytes).expect("surface was not utf-8")
    }

    /// The node's feature string (as a CStr)
    #[inline(always)]
    pub fn feature_cstr(&self) -> &CStr {
        // Safety: self.feature *is* a CStr.
        unsafe { CStr::from_ptr(self.feature) }
    }

    pub fn feature(&self) -> &str {
        self.feature_cstr().to_str().expect("feature was not valid UTF-8")
    }

    #[inline(always)]
    pub fn id(&self) -> u32 {
        self.id.into()
    }

    // don't bother exposing `length` and `rlength` (they're visible via various `surface` functions and that's about their only use)

    /// note: This doesn't seem to actually be useful, it's exposed because it probably exists for *some reason*
    #[inline(always)]
    pub fn rcattr(&self) -> u16 {
        self.rcattr.into()
    }

    /// note: This doesn't seem to actually be useful, it's exposed because it probably exists for *some reason*
    #[inline(always)]
    pub fn lcattr(&self) -> u16 {
        self.lcattr.into()
    }

    /// The ID for the part of speech that this node uses.
    /// note: This doesn't seem to actually be useful, it's exposed because it probably exists for *some reason*
    pub fn pos_id(&self) -> u16 {
        self.posid.into()
    }

    /// This node's character type.
    pub fn char_type(&self) -> u8 {
        self.char_type.into()
    }

    /// status of this `Node`.
    pub fn stat(&self) -> NodeStat {
        match self.stat {
            0 => NodeStat::Normal,
            1 => NodeStat::Unknown,
            2 => NodeStat::BOS,
            3 => NodeStat::EOS,
            4 => NodeStat::EON,
            _ => panic!("Unknown node stat type"),
        }
    }

    #[inline(always)]
    pub fn is_best(&self) -> bool {
        self.isbest != 0
    }

    /// forward accumulative log summation.
    /// This value is only available when MECAB_MARGINAL_PROB is passed.
    #[inline(always)]
    pub fn alpha(&self) -> f32 {
        self.alpha
    }

    /// backward accumulative log summation.
    /// This value is only available when MECAB_MARGINAL_PROB is passed.
    #[inline(always)]
    pub fn beta(&self) -> f32 {
        self.beta
    }

    /// marginal probability.
    /// This value is only available when MECAB_MARGINAL_PROB is passed.
    #[inline(always)]
    pub fn prob(&self) -> f32 {
        self.prob
    }

    #[inline(always)]
    pub fn word_cost(&self) -> i16 {
        self.wcost.into()
    }

    #[inline(always)]
    pub fn cost(&self) -> i64 {
        self.cost.into()
    }

    pub fn iter_prev(&'a self) -> NodeIter<'a> {
        NodeIter { current: Some(self), mode: Mode::PREV }
    }

    pub fn iter_next(&'a self) -> NodeIter<'a> {
        NodeIter { current: Some(self), mode: Mode::NEXT }
    }

    pub fn iter_enext(&'a self) -> NodeIter<'a> {
        NodeIter { current: Some(self), mode: Mode::ENEXT }
    }

    pub fn iter_bnext(&'a self) -> NodeIter<'a> {
        NodeIter { current: Some(self), mode: Mode::BNEXT }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum NodeStat {
    /// Normal node defined in the dictionary.
    /// MECAB_NOR_NODE
    Normal = 0,

    /// Unknown node not defined in the dictionary.
    /// MECAB_UNK_NODE
    Unknown = 1,

    /// Virtual node representing a beginning of the sentence.
    /// MECAB_BOS_NODE
    BOS = 2,

    /// Virtual node representing a end of the sentence.
    /// MECAB_EOS_NODE
    EOS = 3,

    /// Virtual node representing a beginning of the sentence.
    /// MECAB_EON_NODE
    EON = 4,
}

impl NodeStat {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Normal => "MECAB_NOR_NODE",
            Self::Unknown => "MECAB_UNK_NODE",
            Self::BOS => "MECAB_BOS_NODE",
            Self::EOS => "MECAB_EOS_NODE",
            Self::EON => "MECAB_EON_NODE",
        }
    }
}

impl fmt::Display for NodeStat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

enum Mode {
    NEXT,
    PREV,
    ENEXT,
    BNEXT,
}

pub struct NodeIter<'a> {
    current: Option<&'a Node<'a>>,
    mode: Mode,
}

impl<'a> Iterator for NodeIter<'a> {
    type Item = &'a Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let old = self.current.take()?;

        self.current = match self.mode {
            Mode::NEXT => old.next,
            Mode::PREV => old.prev,
            Mode::ENEXT => old.enext,
            Mode::BNEXT => old.bnext,
        };

        Some(old)
    }
}
