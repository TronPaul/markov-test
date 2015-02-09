package markov_text;

import clojure.lang.Symbol;

public class UnknownDirectionException extends IllegalArgumentException {
    public UnknownDirectionException(Symbol direction) {
        super("Unknown direction: " + direction);
    }
}
