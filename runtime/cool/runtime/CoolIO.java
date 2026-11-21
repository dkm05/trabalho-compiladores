package cool.runtime;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class CoolIO extends CoolObject {
    private static InputStream cachedInStream = null;
    private static BufferedReader cachedReader = null;

    public CoolIO() {
    }

    private static synchronized BufferedReader getReader() {
        if (cachedReader == null || cachedInStream != System.in) {
            cachedInStream = System.in;
            cachedReader = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        }
        return cachedReader;
    }

    @Override
    public CoolObject cool$$newInstance() {
        CoolIO obj = new CoolIO();
        obj.cool$$init();
        return obj;
    }

    public CoolObject cool$out_string(CoolString x) {
        return cool$static$out_string(this, x);
    }

    public static CoolObject cool$static$out_string(CoolIO self, CoolString x) {
        if (x == null) {
            CoolRuntime.runtimeError("IO.out_string called with void argument.");
        }
        System.out.print(x.value);
        System.out.flush();
        return self;
    }

    public CoolObject cool$out_int(CoolInt x) {
        return cool$static$out_int(this, x);
    }

    public static CoolObject cool$static$out_int(CoolIO self, CoolInt x) {
        if (x == null) {
            CoolRuntime.runtimeError("IO.out_int called with void argument.");
        }
        System.out.print(x.value);
        System.out.flush();
        return self;
    }

    public CoolString cool$in_string() {
        return cool$static$in_string(this);
    }

    public static CoolString cool$static$in_string(CoolIO self) {
        try {
            String line = getReader().readLine();
            if (line == null || line.indexOf('\0') >= 0) {
                return CoolString.EMPTY;
            }
            return CoolString.of(line);
        } catch (IOException e) {
            return CoolString.EMPTY;
        }
    }

    public CoolInt cool$in_int() {
        return cool$static$in_int(this);
    }

    /**
     * Cool Reference Manual §8.2:
     * Reads a single integer, skipping leading whitespace, optional '+'/'-' sign,
     * and reading the maximal prefix of decimal digits. Any remaining characters
     * up to and including the newline are discarded. Returns 0 on EOF or if no
     * valid integer prefix is present.
     */
    public static CoolInt cool$static$in_int(CoolIO self) {
        try {
            String line = getReader().readLine();
            if (line == null) {
                return CoolInt.ZERO;
            }
            int len = line.length();
            int idx = 0;
            while (idx < len && isCoolSpace(line.charAt(idx))) {
                idx++;
            }
            if (idx >= len) {
                return CoolInt.ZERO;
            }
            int signStart = idx;
            char first = line.charAt(idx);
            if (first == '+' || first == '-') {
                idx++;
            }
            int digitStart = idx;
            while (idx < len) {
                char c = line.charAt(idx);
                if (c >= '0' && c <= '9') {
                    idx++;
                } else {
                    break;
                }
            }
            if (idx == digitStart) {
                return CoolInt.ZERO;
            }
            String token = line.substring(signStart, idx);
            try {
                return CoolInt.of(Integer.parseInt(token));
            } catch (NumberFormatException e) {
                return CoolInt.ZERO;
            }
        } catch (IOException e) {
            return CoolInt.ZERO;
        }
    }

    private static boolean isCoolSpace(char c) {
        return c == ' ' || c == '\t' || c == '\r' || c == '\f' || c == '\013';
    }
}
