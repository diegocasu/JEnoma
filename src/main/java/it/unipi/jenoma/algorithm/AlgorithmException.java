package it.unipi.jenoma.algorithm;

public class AlgorithmException  extends RuntimeException{

    private static final long serialVersionUID = 7816138221965202687L;

    /**
     * Creates an <code>AlgorithmException</code> providing an error message and an exception cause.
     *
     * @param msg	the error message
     * @param cause	the error cause
     */
    public AlgorithmException(String msg,Throwable cause) {
        super(msg,cause);
    }

    /**
     * Creates an <code>AlgorithmException</code> providing only an error message
     *
     * @param msg	the error message
     */
    public AlgorithmException(String msg){
        this(msg,null);
    }

    /**
     * Creates an <code>AlgorithmException</code> providing an exception cause, and no additional message.
     *
     * @param cause the error cause
     */
    public AlgorithmException(Throwable cause){
        this("",cause);
    }

}
