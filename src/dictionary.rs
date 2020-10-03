use std::{
    ffi::CStr,
    fmt,
    os::raw::{c_char, c_int, c_uint, c_ushort},
};

#[repr(C)]
pub struct DictionaryInfo<'a> {
    filename: *const c_char,
    charset: *const c_char,
    size: c_uint,
    dict_type: c_int,
    lsize: c_uint,
    rsize: c_uint,
    version: c_ushort,
    pub next: Option<&'a DictionaryInfo<'a>>,
}

impl<'a> DictionaryInfo<'a> {
    /// filename of dictionary
    /// On Windows, filename is stored in UTF-8 encoding
    #[inline(always)]
    pub fn filename_cstr(&self) -> &'a CStr {
        // Safety:
        // * filename must always be a valid aligned, non-null pointer.
        // * ... To a C style String of bytes.
        unsafe { CStr::from_ptr(self.filename) }
    }

    /// filename of dictionary
    /// On Windows, filename is stored in UTF-8 encoding
    /// # Panics
    /// If `charset` isn't a valid CStr
    pub fn filename(&self) -> &'a str {
        self.filename_cstr().to_str().expect("invalid UTF-8")
    }

    /// character set of the dictionary. e.g., "SHIFT-JIS", "UTF-8"
    /// (as a cstring)
    #[inline(always)]
    pub fn charset_cstr(&self) -> &'a CStr {
        // Safety:
        // * charset must always be a valid aligned, non-null pointer.
        // * ... To a C style String of bytes.
        unsafe { CStr::from_ptr(self.charset) }
    }

    /// character set of the dictionary. e.g., "SHIFT-JIS", "UTF-8"
    /// (as a cstring)
    /// # Panics
    /// If `charset` isn't a valid CStr
    pub fn charset(&self) -> &'a str {
        self.charset_cstr().to_str().expect("invalid UTF-8")
    }

    /// How many words are registered in this dictionary.
    #[inline(always)]
    pub fn size(&self) -> u64 {
        self.size.into()
    }

    /// dictionary type
    /// this value should be MECAB_USR_DIC, MECAB_SYS_DIC, or MECAB_UNK_DIC.
    #[inline(always)]
    pub fn dict_type(&self) -> DictionaryInfoKind {
        match self.dict_type {
            0 => DictionaryInfoKind::System,
            1 => DictionaryInfoKind::User,
            2 => DictionaryInfoKind::Unknown,
            _ => unreachable!("Mecab doesn't return anything else."),
        }
    }

    /// left attributes size
    #[inline(always)]
    pub fn left_attrs_size(&self) -> u64 {
        self.lsize.into()
    }

    /// right attributes size
    #[inline(always)]
    pub fn right_attrs_size(&self) -> u64 {
        self.rsize.into()
    }

    /// version of this dictionary
    #[inline(always)]
    pub fn version(&self) -> u32 {
        self.version.into()
    }

    /// Returns an iterator of `&self`
    pub fn iter(&self) -> DictIter<'_> {
        DictIter { current: Some(self) }
    }
}

pub struct DictIter<'a> {
    current: Option<&'a DictionaryInfo<'a>>,
}

impl<'a> Iterator for DictIter<'a> {
    type Item = &'a DictionaryInfo<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let old = self.current.take()?;

        self.current = old.next;

        Some(old)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum DictionaryInfoKind {
    /// This is a system dictionary.
    /// MECAB_SYS_DIC
    System = 0,

    /// This is a user dictionary.
    /// MECAB_USR_DIC
    User = 1,

    /// This is a unknown word dictionary.
    /// MECAB_UNK_DIC
    Unknown = 2,
}

impl DictionaryInfoKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::System => "MECAB_SYS_DIC",
            Self::User => "MECAB_USR_DIC",
            Self::Unknown => "MECAB_UNK_DIC",
        }
    }
}

impl fmt::Display for DictionaryInfoKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
