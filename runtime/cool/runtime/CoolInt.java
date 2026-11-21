package cool.runtime;

public final class CoolInt extends CoolObject {
    private static final int CACHE_LOW = -128;
    private static final int CACHE_HIGH = 127;
    private static final CoolInt[] CACHE = new CoolInt[CACHE_HIGH - CACHE_LOW + 1];

    static {
        for (int i = 0; i < CACHE.length; i++) {
            CACHE[i] = new CoolInt(i + CACHE_LOW);
        }
    }

    public static final CoolInt ZERO = CACHE[-CACHE_LOW];

    public final int value;

    private CoolInt(int value) {
        this.value = value;
    }

    public static CoolInt of(int value) {
        if (value >= CACHE_LOW && value <= CACHE_HIGH) {
            return CACHE[value - CACHE_LOW];
        }
        return new CoolInt(value);
    }

    @Override
    public CoolObject cool$$newInstance() {
        return ZERO;
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }
}
