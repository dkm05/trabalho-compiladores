package cool.runtime;

public final class CoolBool extends CoolObject {
    public static final CoolBool TRUE = new CoolBool(true);
    public static final CoolBool FALSE = new CoolBool(false);

    public final boolean value;

    private CoolBool(boolean value) {
        this.value = value;
    }

    public static CoolBool of(boolean value) {
        return value ? TRUE : FALSE;
    }

    @Override
    public CoolObject cool$$newInstance() {
        return FALSE;
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }
}
