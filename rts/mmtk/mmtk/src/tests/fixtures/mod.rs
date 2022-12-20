use atomic_refcell::AtomicRefCell;
use std::sync::Once;

use mmtk::AllocationSemantics;
use mmtk::util::{ObjectReference, VMThread, VMMutatorThread};

use crate::api::*;
use crate::object_model::OBJECT_REF_OFFSET;

pub trait FixtureContent {
    fn create() -> Self;
}

pub struct Fixture<T: FixtureContent> {
    content: AtomicRefCell<Option<Box<T>>>,
    once: Once,
}

unsafe impl<T: FixtureContent> Sync for Fixture<T> {}

impl<T: FixtureContent> Fixture<T> {
    pub const fn new() -> Self {
        Self {
            content: AtomicRefCell::new(None),
            once: Once::new(),
        }
    }

    pub fn with_fixture<F: Fn(&T)>(&self, func: F) {
        self.once.call_once(|| {
            let content = Box::new(T::create());
            let mut borrow = self.content.borrow_mut();
            *borrow = Some(content);
        });
        {
            let borrow = self.content.borrow();
            func(borrow.as_ref().unwrap())
        }
    }
}

pub struct SingleObject {
    pub objref: ObjectReference,
}

impl FixtureContent for SingleObject {
    fn create() -> Self {
        const MB: usize = 1024 * 1024;
        // 1MB heap
        mmtk_gc_init(MB);
        mmtk_initialize_collection(VMThread::UNINITIALIZED);
        // Make sure GC does not run during test.
        mmtk_disable_collection();
        let handle = mmtk_bind_mutator(VMMutatorThread(VMThread::UNINITIALIZED));

        // A relatively small object, typical for Ruby.
        let size = 40;
        let semantics = AllocationSemantics::Default;

        let addr = mmtk_alloc(handle, size, 8, 0, semantics);
        assert!(!addr.is_zero());

        let objref = unsafe { addr.add(OBJECT_REF_OFFSET).to_object_reference() };
        mmtk_post_alloc(handle, objref, size, semantics);

        SingleObject { objref }
    }
}
