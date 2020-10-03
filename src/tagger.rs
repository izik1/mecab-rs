use std::{
    ffi::{c_void, CStr, CString},
    marker::PhantomData,
    os::raw::c_char,
    ptr::NonNull,
};

use crate::{dictionary::DictionaryInfo, node::Node2, Lattice};

/// # Panics
/// If the given CStr ends up not being a valid UTF-8 string
fn map_res_str<'a>(res: Result<&'a CStr, Option<&'a CStr>>) -> Result<&'a str, Option<&'a str>> {
    match res {
        Ok(s) => Ok(s.to_str().expect("String was not valid UTF-8")),
        Err(Some(e)) => Err(Some(e.to_str().expect("String was not valid UTF-8"))),
        Err(None) => Err(None),
    }
}

pub struct Tagger {
    inner: NonNull<c_void>,
}

unsafe impl Send for Tagger {}
unsafe impl Sync for Tagger {}

impl Tagger {
    /// # Safety
    /// This function assumes that `inner` is a valid pointer to a mecab object.
    pub(crate) unsafe fn from_ptr(raw: NonNull<c_void>) -> Self {
        Self { inner: raw }
    }

    // todo: fix memory leak
    pub fn new<T: Into<Vec<u8>>>(arg: T) -> Result<Tagger, Option<CString>> {
        unsafe {
            let inner = NonNull::new(crate::mecab_new2(crate::str_to_ptr(
                &CString::new(arg).unwrap(),
            )));

            let inner = match inner {
                Some(inner) => inner,
                None => return Err(crate::global_last_error()),
            };

            Ok(Tagger { inner })
        }
    }

    /// uses a unique reference to `self` to get the last error reported.
    /// the unique reference ensures that nothing invalidates it, if you only have a `&self`, use [`last_error`] instead.
    pub fn last_error_ref(&mut self) -> Option<&CStr> {
        // Safety:
        // `self.inner` is guaranteed to be valid (library invariant)
        // this borrow is tied to `&mut self`, satisfying that requirement.
        unsafe { crate::last_error(self.inner.as_ptr()) }
    }

    pub fn last_error(&self) -> Option<CString> {
        // Safety:
        // `self.inner` is guaranteed to be valid (library invariant)
        // this borrow is immediately dropped (see `global_last_error` to see discussion on if this is sound or not due to not having a mutable reference)
        unsafe { crate::last_error(self.inner.as_ptr()) }.map(ToOwned::to_owned)
    }

    /// Return true if partial parsing mode is on.
    #[deprecated(note = "use `Lattice::has_request_type(MECAB_PARTIAL)` instead")]
    pub fn partial(&self) -> bool {
        unsafe { crate::mecab_get_partial(self.inner.as_ptr()) != 0 }
    }

    /// Set partial parsing mode.
    #[deprecated(note = "use `Lattice::{add,remove}_request_type(MECAB_PARTIAL)`")]
    pub fn set_partial(&mut self, partial: bool) {
        unsafe {
            crate::mecab_set_partial(self.inner.as_ptr(), partial as i32);
        }
    }

    /// Return temparature parameter theta.
    pub fn theta(&self) -> f32 {
        unsafe { crate::mecab_get_theta(self.inner.as_ptr()) }
    }

    /// Set temparature parameter theta.
    pub fn set_theta(&mut self, theta: f32) {
        unsafe {
            crate::mecab_set_theta(self.inner.as_ptr(), theta);
        }
    }

    /// Get lattice level
    #[deprecated(note = "use `Lattice::*_request_type()`")]
    pub fn lattice_level(&self) -> i32 {
        unsafe { crate::mecab_get_lattice_level(self.inner.as_ptr()) }
    }

    /// Set lattice level
    #[deprecated(note = "use `Lattice::*_request_type()`")]
    pub fn set_lattice_level(&mut self, level: i32) {
        unsafe {
            crate::mecab_set_lattice_level(self.inner.as_ptr(), level);
        }
    }

    /// Return true if all morphs output mode is on.
    #[deprecated(note = "use `Lattice::has_request_type(MECAB_ALL_MORPHS)`")]
    pub fn all_morphs(&self) -> bool {
        unsafe { crate::mecab_get_all_morphs(self.inner.as_ptr()) != 0 }
    }

    #[deprecated(note = "use `Lattice::{add,remove}_request_type(MECAB_ALL_MORPHS)`")]
    pub fn set_all_morphs(&mut self, all_morphs: i32) {
        unsafe {
            crate::mecab_set_all_morphs(self.inner.as_ptr(), all_morphs);
        }
    }

    pub fn parse(&self, latice: &Lattice) -> bool {
        unsafe { crate::mecab_parse_lattice(self.inner.as_ptr(), latice.inner) != 0 }
    }

    /// # Panics
    /// If the returned string is *not* valid UTF-8.
    pub fn parse_to_str<'a, T: AsRef<[u8]> + ?Sized>(
        &'a mut self,
        input: &'a T,
    ) -> Result<&str, Option<&str>> {
        map_res_str(self.parse_to_cstr(input))
    }

    /// # Panics
    /// If `input` contains any null bytes
    pub fn parse_to_cstr<'a, T: AsRef<[u8]> + ?Sized>(
        &'a mut self,
        input: &'a T,
    ) -> Result<&'a CStr, Option<&'a CStr>> {
        let input = input.as_ref();
        assert!(
            input.iter().all(|&byte| byte != 0),
            "BUG: null byte detected in input string; a proper error hasn't been created yet."
        );

        // note: we use `mecab_sparse_tostr2` to avoid calculating a length for no reason ()
        // Safety:
        // requires exclusive access to `self.inner` (for the length of the function call)
        // *might* require there to be no null bytes inside the `input` string. (assume yes until proven otherwise)
        // `input` must live for the length of the call.
        let res = unsafe {
            crate::mecab_sparse_tostr2(
                self.inner.as_ptr(),
                input.as_ptr() as *const c_char,
                input.len(),
            )
        };

        match res.is_null() {
            true => Err(self.last_error_ref()),
            // Safety:
            // `res` isn't null, so we assume that `mecab_sparse_tostr2` returned valid data.
            // documentation of the function claims that `res` is valid until the next parse function is called.
            // So we can tie it to `&mut 'a self`
            // Note that the above is overly strict, it's only calls to parse functions on self that require this,
            // but we can't do anything about that.
            // It isn't clear if it requires `input` to also remain valid, so we assume it does.
            false => unsafe { Ok(CStr::from_ptr(res)) },
        }
    }

    /// Parse given sentence and return Node object.
    /// You should not delete the returned node object. The returned buffer
    /// is overwritten when parse method is called again.
    /// You can traverse all nodes via Node::next member.
    /// This method is NOT thread safe.
    pub fn parse_to_node<'a, T: AsRef<[u8]> + ?Sized>(
        &'a mut self,
        input: &'a T,
    ) -> Result<&'a Node2<'a>, Option<&'a CStr>> {
        let input = input.as_ref();
        assert!(
            input.iter().all(|&byte| byte != 0),
            "BUG: null byte detected in input string; a proper error hasn't been created yet."
        );

        // note: we use `mecab_sparse_tonode2` to avoid calculating a length for no reason ()
        // Safety:
        // requires exclusive access to `self.inner` (for the length of the function call)
        // *might* require there to be no null bytes inside the `input` string. (assume yes until proven otherwise)
        // `input` must live for the length of the call.
        let res = unsafe {
            crate::mecab_sparse_tonode2(
                self.inner.as_ptr(),
                input.as_ptr() as *const c_char,
                input.len(),
            )
        };

        match res.is_null() {
            true => Err(self.last_error_ref()),
            // Safety:
            // * `res` isn't null, so we assume that `mecab_sparse_tonode2` returned valid data.
            // * documentation of the function claims that `res` is valid until the next parse function is called,
            // so we can tie it to `&mut 'a self`
            // * Note that the above is overly strict, it's only calls to parse functions on self that require this,
            // but we can't do anything about that.
            // * definitely requires the input string to live for the length of the call.
            false => unsafe { Ok(&*(res as *const _)) },
        }
    }

    /// Parse given sentence and obtain N-best results as a string format.
    /// Currently, N must be 1 <= N <= 512 due to the limitation of the buffer size.
    /// You should not delete the returned string. The returned buffer
    /// is overwritten when parse method is called again.
    /// This method is DEPRECATED. Use Lattice class.
    /// # Panics
    /// If `input` contains any null bytes
    #[deprecated(note = "use `Lattice`")]
    pub fn parse_nbest_to_cstr<'a, T: AsRef<[u8]> + ?Sized>(
        &'a mut self,
        n: usize,
        input: &'a T,
    ) -> Result<&'a CStr, Option<&'a CStr>> {
        let input = input.as_ref();
        assert!(
            input.iter().all(|&byte| byte != 0),
            "BUG: null byte detected in input string; a proper error hasn't been created yet."
        );

        // note: we use `mecab_nbest_sparse_tostr2` to avoid calculating a length for no reason ()
        // Safety:
        // requires exclusive access to `self.inner` (for the length of the function call)
        // *might* require there to be no null bytes inside the `input` string. (assume yes until proven otherwise)
        // `input` must live for the length of the call.
        let res = unsafe {
            crate::mecab_nbest_sparse_tostr2(
                self.inner.as_ptr(),
                n,
                input.as_ptr() as *const c_char,
                input.len(),
            )
        };

        match res.is_null() {
            true => Err(self.last_error_ref()),
            // Safety:
            // `res` isn't null, so we assume that `mecab_nbest_sparse_tostr2` returned valid data.
            // documentation of the function claims that `res` is valid until the next parse function is called.
            // So we can tie it to `&mut 'a self`
            // Note that the above is overly strict, it's only calls to parse functions on self that require this,
            // but we can't do anything about that.
            // It isn't clear if it requires `input` to also remain valid, so we assume it does.
            false => unsafe { Ok(CStr::from_ptr(res)) },
        }
    }

    /// # Panics
    /// If the returned string is *not* valid UTF-8.
    #[deprecated(note = "use `Lattice`")]
    pub fn parse_nbest_to_str<'a, T: AsRef<[u8]> + ?Sized>(
        &'a mut self,
        n: usize,
        input: &'a T,
    ) -> Result<&str, Option<&str>> {
        #[allow(deprecated)]
        map_res_str(self.parse_nbest_to_cstr(n, input))
    }

    #[deprecated(note = "use `Lattice`")]
    pub fn parse_nbest_init<'a, T: AsRef<[u8]> + ?Sized>(
        &'a mut self,
        input: &'a T,
    ) -> Result<NBest<'a>, Option<&'a CStr>> {
        let input = input.as_ref();
        assert!(
            input.iter().all(|&byte| byte != 0),
            "BUG: null byte detected in input string; a proper error hasn't been created yet."
        );

        // note: we use `mecab_nbest_init2` to avoid calculating a length for no reason ()
        // Safety:
        // requires exclusive access to `self.inner` (for the length of the function call)
        // *might* require there to be no null bytes inside the `input` string. (assume yes until proven otherwise)
        // `input` must live for the length of the call.
        let res = unsafe {
            crate::mecab_nbest_init2(
                self.inner.as_ptr(),
                input.as_ptr() as *const c_char,
                input.len(),
            ) == 1
        };

        match res {
            false => Err(self.last_error_ref()),
            true => Ok(NBest {
                tagger: self,
                _input: PhantomData,
            }),
        }
    }

    /// Return formatted node object. The format is specified with
    /// --unk-format, --bos-format, --eos-format, and --eon-format respectively.
    /// You should not delete the returned string. The returned buffer
    /// is overwritten when parse method is called again.
    /// This method is NOT thread safe.
    /// This method is DEPRECATED. Use Lattice class.
    #[deprecated(note = "use `Lattice`")]
    pub fn format_node_to_cstr<'a>(
        &'a mut self,
        node: &Node2,
    ) -> Result<&'a CStr, Option<&'a CStr>> {
        let res = unsafe {
            crate::mecab_format_node(self.inner.as_ptr(), node as *const Node2 as *const _)
        };

        match res.is_null() {
            true => Err(self.last_error_ref()),
            false => unsafe { Ok(CStr::from_ptr(res)) },
        }
    }

    /// # Panics
    /// If the returned string is *not* valid UTF-8.
    #[deprecated(note = "use `Lattice`")]
    pub fn format_node_to_str<'a>(&'a mut self, node: &Node2) -> Result<&'a str, Option<&'a str>> {
        #[allow(deprecated)]
        map_res_str(self.format_node_to_cstr(node))
    }

    pub fn dictionary_info<'a>(&'a mut self) -> &'a DictionaryInfo<'a> {
        unsafe { &*(crate::mecab_dictionary_info(self.inner.as_ptr()) as *const DictionaryInfo) }
    }
}

impl Drop for Tagger {
    fn drop(&mut self) {
        unsafe {
            crate::mecab_destroy(self.inner.as_ptr());
        }
    }
}

/// Safety Note: While this struct is in use, it *cannot* call any of the parse methods on it.
pub struct NBest<'a> {
    tagger: &'a mut Tagger,
    // while we don't directly need access to this, we need to ensure that we know to keep that around.
    _input: PhantomData<&'a [u8]>,
}

impl<'a> NBest<'a> {
    pub fn next_cstr(&mut self) -> Option<&CStr> {
        // Safety:
        // requires exclusive access to `self.tagger.inner` (for the length of the function call)
        let res = unsafe { crate::mecab_nbest_next_tostr(self.tagger.inner.as_ptr()) };

        match res.is_null() {
            true => None,
            // Safety:
            // `res` isn't null, so we assume that `mecab_nbest_next_tostr` returned valid data.
            // documentation of the function claims that `res` is valid until the next `next_*` function is called.
            // So we can tie it to `&mut 'a self`
            false => unsafe { Some(CStr::from_ptr(res)) },
        }
    }

    /// # Panics
    /// If the returned string is *not* valid UTF-8.
    pub fn next_str(&mut self) -> Option<&str> {
        self.next_cstr()
            .map(CStr::to_str)
            .transpose()
            .expect("String was not valid UTF-8")
    }

    pub fn next_node(&mut self) -> Option<&'a Node2<'a>> {
        // Safety:
        // requires exclusive access to `self.tagger.inner` (for the length of the function call)
        let res = unsafe { crate::mecab_nbest_next_tonode(self.tagger.inner.as_ptr()) };
        match res.is_null() {
            true => None,
            // Safety:
            // `res` isn't null, so we assume that `mecab_nbest_next_tonode` returned valid data.
            // documentation of the function claims that `res` is valid until the next `next_*` function is called.
            // So we can tie it to `&mut 'a self`
            false => unsafe { Some(&*(res as *const _)) },
        }
    }
}
