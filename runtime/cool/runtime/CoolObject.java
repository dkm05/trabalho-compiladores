package cool.runtime;

public class CoolObject implements Cloneable {
    public CoolObject() {
    }

    public void cool$$init() {
    }

    public CoolObject cool$$newInstance() {
        CoolObject obj = new CoolObject();
        obj.cool$$init();
        return obj;
    }

    public static String coolTypeNameOf(CoolObject obj) {
        Class<?> c = obj.getClass();
        if (c == CoolObject.class) return "Object";
        if (c == CoolIO.class)     return "IO";
        if (c == CoolInt.class)    return "Int";
        if (c == CoolString.class) return "String";
        if (c == CoolBool.class)   return "Bool";
        return c.getName();
    }

    public CoolObject cool$abort() {
        return cool$static$abort(this);
    }

    public static CoolObject cool$static$abort(CoolObject self) {
        String msg = "Abort called from class " + coolTypeNameOf(self);
        if (CoolRuntime.throwOnError) {
            throw new RuntimeException(msg);
        }
        System.out.println(msg);
        System.out.flush();
        System.exit(1);
        return self;
    }

    public CoolString cool$type_name() {
        return cool$static$type_name(this);
    }

    public static CoolString cool$static$type_name(CoolObject self) {
        return CoolString.of(coolTypeNameOf(self));
    }

    public CoolObject cool$copy() {
        return cool$static$copy(this);
    }

    public static CoolObject cool$static$copy(CoolObject self) {
        try {
            return (CoolObject) self.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionError("CoolObject must implement Cloneable", e);
        }
    }
}
