use std::num::Wrapping;

pub struct Prng
{
    s: u64,
}

impl Prng
{
    pub fn new(seed: u64) -> Self
    {
        assert!(seed != 0);
        Self { s: seed }
    }

    pub fn rand64(&mut self) -> u64
    {
        const N: Wrapping<u64> = Wrapping(2_685_821_657_736_338_717);
        let mut s = Wrapping(self.s);
        s ^= s >> 12;
        s ^= s << 25;
        s ^= s >> 27;
        self.s = s.0;
        (s * N).0
    }

    pub fn rand<T>(&mut self) -> T
    where
        T: From<u64>,
    {
        T::from(self.rand64())
    }

    pub fn sparse_rand<T>(&mut self) -> T
    where
        T: From<u64>,
    {
        T::from(self.rand64() & self.rand64() & self.rand64())
    }
}
