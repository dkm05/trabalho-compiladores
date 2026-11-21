package cool.runtime;

public final class CoolRuntime {
    /**
     * When false (default in compiled Cool programs), runtime errors print
     * a diagnostic message to stderr and terminate the JVM with exit code 1.
     * When true (used by in-JVM unit tests), runtime errors throw RuntimeException.
     */
    public static volatile boolean throwOnError = false;

    private CoolRuntime() {
    }

    public static RuntimeException runtimeError(String msg) {
        if (throwOnError) {
            throw new RuntimeException(msg);
        }
        System.err.println(msg);
        System.err.flush();
        System.exit(1);
        return new RuntimeException(msg);
    }

    public static void checkDispatchNotVoid(CoolObject obj, String file, int line) {
        if (obj == null) {
            runtimeError(file + ":" + line + ": Dispatch to void.");
        }
    }

    public static void checkCaseNotVoid(CoolObject obj, String file, int line) {
        if (obj == null) {
            runtimeError(file + ":" + line + ": Match on void in case statement.");
        }
    }

    public static RuntimeException caseNoMatch(CoolObject obj, String file, int line) {
        String typeName = (obj == null) ? "void" : CoolObject.coolTypeNameOf(obj);
        return runtimeError(file + ":" + line + ": No match in case statement for Class " + typeName);
    }

    public static CoolInt div(int a, int b, String file, int line) {
        if (b == 0) {
            runtimeError(file + ":" + line + ": Division by zero.");
        }
        return CoolInt.of(a / b);
    }

    public static CoolBool lt(int a, int b) {
        return CoolBool.of(a < b);
    }

    public static CoolBool leq(int a, int b) {
        return CoolBool.of(a <= b);
    }

    public static CoolBool not(CoolBool b) {
        return CoolBool.of(!b.value);
    }

    public static CoolBool isVoid(CoolObject obj) {
        return CoolBool.of(obj == null);
    }

    public static CoolBool coolEquals(CoolObject a, CoolObject b) {
        if (a == b) {
            return CoolBool.TRUE;
        }
        if (a == null || b == null) {
            return CoolBool.FALSE;
        }
        if (a instanceof CoolInt && b instanceof CoolInt) {
            return CoolBool.of(((CoolInt) a).value == ((CoolInt) b).value);
        }
        if (a instanceof CoolBool && b instanceof CoolBool) {
            return CoolBool.of(((CoolBool) a).value == ((CoolBool) b).value);
        }
        if (a instanceof CoolString && b instanceof CoolString) {
            return CoolBool.of(((CoolString) a).value.equals(((CoolString) b).value));
        }
        return CoolBool.FALSE;
    }
}
