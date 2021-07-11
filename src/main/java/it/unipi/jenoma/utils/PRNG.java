package it.unipi.jenoma.utils;

import org.apache.commons.math3.random.MersenneTwister;


/**
 * A pseudorandom number generator based on the MersenneTwister implementation provided in
 * <code>org.apache.commons.math3.random.MersenneTwister</code>, accepting integer seeds.<br>
 * Each remote machine uses a certain number of PRNGs, seeded differently starting from a base
 * seed provided in the configuration file. More precisely, each machine creates <code>N+1</code> PRNGs:
 * a main generator, used to execute single-threaded tasks in the genetic algorithm,
 * and <code>N = Runtime.getRuntime().availableProcessors()</code> supporting generators,
 * used to execute multi-threaded tasks. The seeding is done according to the following procedure:<br>
 * 1) a <code>hostSeed</code> is computed applying <code>Objects.hash()</code> to the name of the host
 *    and the base seed, both as specified in the configuration file;<br>
 * 2) the main generator is seeded with <code>hostSeed</code>;<br>
 * 3) a supporting generator <code>k</code>, where <code>k = 1...N</code>, is seeded with
 *    <code>hostSeed + k</code>.
 */
public class PRNG {
    private MersenneTwister mersenneTwister;
    private int seed;


    /**
     * Creates a new random number generator using an integer seed.
     * @param seed  the initial seed.
     */
    public PRNG(int seed) {
        this.seed = seed;
        this.mersenneTwister = new MersenneTwister(seed);
    }

    public int getSeed() {
        return seed;
    }

    /**
     * Extracts a pseudorandom uniformly distributed integer value between 0 (inclusive)
     * and the specified value (exclusive).
     * @param n  the upper bound (exclusive) of the number to be returned. It must be positive.
     * @return   a pseudorandom uniformly distributed integer value between 0 (inclusive) and n (exclusive).
     * @throws IllegalArgumentException  if the provided upper bound is invalid.
     */
    public int nextInt(int n) {
        return this.mersenneTwister.nextInt(n);
    }

    /**
     * Extracts a pseudorandom uniformly distributed double value between 0.0 and 1.0.
     * @return  a pseudorandom uniformly distributed double value between 0.0 and 1.0.
     */
    public double nextDouble() {
        return this.mersenneTwister.nextDouble();
    }
}
