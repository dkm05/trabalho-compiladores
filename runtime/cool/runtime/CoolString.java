package cool.runtime;

public final class CoolString extends CoolObject {
    public static final CoolString EMPTY = new CoolString("");

    public final String value;

    private CoolString(String value) {
        this.value = value;
    }

    public static CoolString of(String value) {
        if (value == null || value.isEmpty()) {
            return EMPTY;
        }
        return new CoolString(value);
    }

    @Override
    public CoolObject cool$$newInstance() {
        return EMPTY;
    }

    public CoolInt cool$length() {
        return cool$static$length(this);
    }

    public static CoolInt cool$static$length(CoolString self) {
        return CoolInt.of(self.value.length());
    }

    public CoolString cool$concat(CoolString s) {
        return cool$static$concat(this, s);
    }

    public static CoolString cool$static$concat(CoolString self, CoolString s) {
        if (s == null) {
            CoolRuntime.runtimeError("String.concat called with void argument.");
        }
        return CoolString.of(self.value + s.value);
    }

    public CoolString cool$substr(CoolInt i, CoolInt l) {
        return cool$static$substr(this, i, l);
    }

    public static CoolString cool$static$substr(CoolString self, CoolInt i, CoolInt l) {
        if (i == null || l == null) {
            CoolRuntime.runtimeError("String.substr called with void argument.");
        }
        int start = i.value;
        int len = l.value;
        long end = (long) start + (long) len;
        if (start < 0 || len < 0 || end > (long) self.value.length()) {
            CoolRuntime.runtimeError(
                "String.substr index out of range: start=" + start
                + ", length=" + len + ", string length=" + self.value.length()
            );
        }
        return CoolString.of(self.value.substring(start, (int) end));
    }

    @Override
    public String toString() {
        return value;
    }
}
