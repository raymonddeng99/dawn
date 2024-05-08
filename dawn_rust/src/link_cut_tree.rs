pub struct Node {
    value: i32,
    parent: Option<*mut Node>,
    left: Option<*mut Node>,
    right: Option<*mut Node>,
    rev: bool,
    sum: i32,
}

impl Node {
    pub fn new(value: i32) -> Self {
        Node {
            value,
            parent: None,
            left: None,
            right: None,
            rev: false,
            sum: value,
        }
    }

    pub fn get_sum(&self) -> i32 {
        self.sum
    }

    pub fn update_sum(&mut self) {
        self.sum = self.value
            + self.left.as_ref().map_or(0, |n| unsafe { &**n }.get_sum())
            + self.right.as_ref().map_or(0, |n| unsafe { &**n }.get_sum());
    }

    pub fn push(&mut self) {
        if self.rev {
            self.rev = false;
            std::mem::swap(&mut self.left, &mut self.right);
            if let Some(l) = self.left.as_mut() {
                unsafe { &mut **l }.rev = !unsafe { &mut **l }.rev;
                unsafe { &mut **l }.push();
            }
            if let Some(r) = self.right.as_mut() {
                unsafe { &mut **r }.rev = !unsafe { &mut **r }.rev;
                unsafe { &mut **r }.push();
            }
        }
    }

    pub fn make_root(&mut self) {
        self.push();
        if let Some(p) = self.parent.as_mut() {
            unsafe { &mut **p }.left = None;
            unsafe { &mut **p }.right = None;
            unsafe { &mut **p }.make_root();
            self.parent = None;
        }
    }

    pub fn splay(&mut self) {
        self.make_root();
        let mut p = self.parent;
        while let Some(pp) = p {
            let mut pp_ptr = pp as *mut Node;
            let p_ptr = p as *mut Node;
            unsafe { &mut *p_ptr }.make_root();
            if let Some(ppp) = unsafe { &mut *p_ptr }.parent {
                let ppp_ptr = ppp as *mut Node;
                unsafe { &mut *ppp_ptr }.make_root();
                if (unsafe { &*ppp_ptr }.left.as_ref() == Some(&p_ptr))
                    == (unsafe { &*p_ptr }.left.as_ref() == Some(self))
                {
                    unsafe { &mut *p_ptr }.rotate();
                } else {
                    self.rotate();
                }
            }
            self.rotate();
            p = self.parent;
        }
        self.update_sum();
    }

    fn rotate(&mut self) {
        let p = unsafe { &mut *self.parent.unwrap() };
        let pp = p.parent;
        if let Some(pp_ptr) = pp {
            let pp_mut = unsafe { &mut *pp_ptr };
            if pp_mut.left.as_ref() == Some(p) {
                pp_mut.left = Some(self);
            } else {
                pp_mut.right = Some(self);
            }
        }
        self.parent = pp;
        if p.left.as_ref() == Some(self) {
            p.left = self.right;
            self.right = Some(p);
        } else {
            p.right = self.left;
            self.left = Some(p);
        }
        p.parent = Some(self);
        p.update_sum();
        self.update_sum();
    }

    pub fn access(&mut self) {
        self.splay();
        self.rev = false;
        let mut c = self.left;
        while let Some(c_ptr) = c {
            let mut c_mut = unsafe { &mut *c_ptr };
            c_mut.splay();
            c_mut.rev = !c_mut.rev;
            c_mut.update_sum();
            c_mut.right = Some(self);
            self.parent = Some(c_mut);
            c = self.left;
        }
    }

    pub fn link(&mut self, other: &mut Node) {
        self.access();
        other.access();
        other.parent = Some(self);
        other.update_sum();
    }

    pub fn cut(&mut self) {
        self.access();
        if let Some(l) = self.left.take() {
            unsafe { &mut *l }.parent = None;
            self.update_sum();
        }
    }

    pub fn root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn lca(&mut self, other: &mut Node) -> *mut Node {
        other.access();
        let other_sum = other.get_sum();
        self.access();
        let mut z = other;
        while unsafe { &*z }.get_sum() < other_sum || unsafe { &*z }.get_sum() < self.get_sum() {
            z.access();
            z = unsafe { &mut *z }.parent.unwrap();
        }
        z
    }

    pub fn path_sum(&mut self, other: &mut Node) -> i32 {
        let z = self.lca(other);
        unsafe { &mut *self }.access();
        unsafe { &mut *other }.access();
        let x_sum = self.get_sum();
        let y_sum = other.get_sum();
        let z_sum = unsafe { &*z }.get_sum();
        x_sum + y_sum - 2 * z_sum
    }
}