pub trait Top {
    fn top() -> Self;
}

pub trait Bottom {
    fn bottom() -> Self;
}

pub trait Meet {
    fn min(&self, other: &Self) -> Self;
}

pub trait Join {
    fn max(&self, other: &Self) -> Self;
}

