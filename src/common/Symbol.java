package common;/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:54
 */

/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:54
 */
public class Symbol {
    public static final int TEMP = -1;
    public static final int SINGLE_INT = 0;
    public static final int SINGLE_FLOAT = 1;
    public static final int TRUE = 2;
    public static final int FALSE = 3;

    private String name;
    private int type;
    private Value value;
    private int level;
    private Symbol next;


    public Symbol(String name, int type, int level) {
        this.name = name;
        this.type = type;
        this.level = level;
        this.next = null;
        this.value = new Value(type);
    }

    /**
     * 给int类型创造symbol
     * @param name
     * @param type
     * @param level
     * @param value
     */
    public Symbol(String name, int type, int level, int value) {
        this(name, type, level);
        this.value.setIntValue(value);
    }

    /**
     * 给float类型创造symbol
     * @param name
     * @param type
     * @param level
     * @param value
     */

    public Symbol(String name, int type, int level, double value) {
        this(name, type, level);
        this.value.setFloatValue(value);
    }

    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public int getType() {
        return type;
    }
    public void setType(int type) {
        this.type = type;
    }
    public Value getValue() {
        return value;
    }
    public void setValue(Value value) {
        this.value = value;
    }
    public int getLevel() {
        return level;
    }
    public void setLevel(int level) {
        this.level = level;
    }
    public Symbol getNext() {
        return next;
    }
    public void setNext(Symbol next) {
        this.next = next;
    }
}
