package it.unipi.jenoma.utils;

import org.apache.commons.math3.random.MersenneTwister;

public class PRNG {
    private MersenneTwister mersenneTwister;
    private int seed;

    public PRNG(int seed) {
        this.seed = seed;
        this.mersenneTwister = new MersenneTwister(seed);
    }

    public int getSeed() {
        return seed;
    }

    public int nextInt(int n) {
        return this.mersenneTwister.nextInt(n);
    }

}
