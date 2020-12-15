package common;/**
 * @Author: zlxx
 * @Date: 2020/12/13 16:28
 */

/**
 * @Author: zlxx
 * @Date: 2020/12/13 16:28
 * 用于存储符号表中符号对应的值,包括int float
 */


public class Value {
    //类型
    private int type;
    private int intValue;
    private double floatValue;


    public Value(boolean bool) {
        if (bool) {
            this.type = Symbol.TRUE;
        } else {
            this.type = Symbol.FALSE;
        }
    }
    public int getType() {
        return type;
    }
    public void setType(int type) {
        this.type = type;
    }
    public int getIntValue() {
        return intValue;
    }
    public void setIntValue(int intValue) {
        this.intValue = intValue;
    }
    public double getFloatValue() {
        return floatValue;
    }
    public void setFloatValue(double floatValue) {
        this.floatValue = floatValue;
    }
    public Value(int type) {
        this.type = type;
    }

    @Override
    public String toString() {
        switch (type) {
            case Symbol.SINGLE_INT:
                return intValue + "";
            case Symbol.SINGLE_FLOAT:
                return floatValue + "";
            case Symbol.TRUE:
                return "true";
            case Symbol.FALSE:
                return "false";
            default:
                return "wrong";
        }
    }

    public Value toFloat() {
        if (type != Symbol.SINGLE_FLOAT) {
            type = Symbol.SINGLE_FLOAT;
            floatValue = (int) intValue;
            intValue = 0;
        }
        return this;
    }

    public Value PLUS(Value value) throws InterpretException {
        if (this.type == Symbol.SINGLE_FLOAT) {
            Value rv = new Value(Symbol.SINGLE_FLOAT);
            if (value.type == Symbol.SINGLE_INT) {
                rv.setFloatValue(this.floatValue + value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                rv.setFloatValue(this.floatValue + value.floatValue);
                return rv;
            }
        } else if (this.type == Symbol.SINGLE_INT) {
            if (value.type == Symbol.SINGLE_INT) {
                Value rv = new Value(Symbol.SINGLE_INT);
                rv.setIntValue(this.intValue + value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                Value rv = new Value(Symbol.SINGLE_FLOAT);
                rv.setFloatValue(this.intValue + value.floatValue);
                return rv;
            }
        }
        throw new InterpretException("算数运算非法");
    }

    public Value MINUS(Value value) throws InterpretException {
        if (this.type == Symbol.SINGLE_FLOAT) {
            Value rv = new Value(Symbol.SINGLE_FLOAT);
            if (value.type == Symbol.SINGLE_INT) {
                rv.setFloatValue(this.floatValue - value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                rv.setFloatValue(this.floatValue - value.floatValue);
                return rv;
            }
        } else if (this.type == Symbol.SINGLE_INT) {
            if (value.type == Symbol.SINGLE_INT) {
                Value rv = new Value(Symbol.SINGLE_INT);
                rv.setIntValue(this.intValue - value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                Value rv = new Value(Symbol.SINGLE_FLOAT);
                rv.setFloatValue(this.intValue - value.floatValue);
                return rv;
            }
        }
        throw new InterpretException("算数运算非法");
    }

    public Value MUL(Value value) throws InterpretException {
        if (this.type == Symbol.SINGLE_FLOAT) {
            Value rv = new Value(Symbol.SINGLE_FLOAT);
            if (value.type == Symbol.SINGLE_INT) {
                rv.setFloatValue(this.floatValue * value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                rv.setFloatValue(this.floatValue * value.floatValue);
                return rv;
            }
        } else if (this.type == Symbol.SINGLE_INT) {
            if (value.type == Symbol.SINGLE_INT) {
                Value rv = new Value(Symbol.SINGLE_INT);
                rv.setIntValue(this.intValue * value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                Value rv = new Value(Symbol.SINGLE_FLOAT);
                rv.setFloatValue(this.intValue * value.floatValue);
                return rv;
            }
        }
        throw new InterpretException("算数运算非法");
    }

    public Value DIV(Value value) throws InterpretException {
        if (this.type == Symbol.SINGLE_FLOAT) {
            Value rv = new Value(Symbol.SINGLE_FLOAT);
            if (value.type == Symbol.SINGLE_INT) {
                if (value.getIntValue() == 0) {
                    throw new InterpretException("不能除0");
                }
                rv.setFloatValue(this.floatValue / value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                if (value.getFloatValue() == 0) {
                    throw new InterpretException("不能除0");
                }
                rv.setFloatValue(this.floatValue / value.floatValue);
                return rv;
            }
        } else if (this.type == Symbol.SINGLE_INT) {
            if (value.type == Symbol.SINGLE_INT) {
                if (value.getIntValue() == 0) {
                    throw new InterpretException("不能除0");
                }
                Value rv = new Value(Symbol.SINGLE_INT);
                rv.setIntValue(this.intValue / value.intValue);
                return rv;
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                if (value.getFloatValue() == 0) {
                    throw new InterpretException("不能除0");
                }
                Value rv = new Value(Symbol.SINGLE_FLOAT);
                rv.setFloatValue(this.intValue / value.floatValue);
                return rv;
            }
        }
        throw new InterpretException("算数运算非法");
    }

    public Value GT(Value value) throws InterpretException {
        if (this.type == Symbol.SINGLE_INT) {
            if (value.type == Symbol.SINGLE_INT) {
                return new Value(this.intValue > value.intValue);
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                return new Value(this.intValue > value.floatValue);
            }
        } else if (this.type == Symbol.SINGLE_FLOAT) {
            if (value.type == Symbol.SINGLE_INT) {
                return new Value(this.floatValue > value.intValue);
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                return new Value(this.floatValue > value.floatValue);
            }
        }
        throw new InterpretException("逻辑比较非法");
    }

    public Value LT(Value value) throws InterpretException {
        return NOT(this.GET(value));
    }

    public Value EQ(Value value) throws InterpretException {
        if (this.type == Symbol.SINGLE_INT) {
            if (value.type == Symbol.SINGLE_INT) {
                return new Value(this.intValue == value.intValue);
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                return new Value(this.intValue == value.floatValue);
            }
        } else if (this.type == Symbol.SINGLE_FLOAT) {
            if (value.type == Symbol.SINGLE_INT) {
                return new Value(this.floatValue == value.intValue);
            } else if (value.type == Symbol.SINGLE_FLOAT) {
                return new Value(this.floatValue == value.floatValue);
            }
        }
        throw new InterpretException("逻辑比较非法");
    }

    public Value GET(Value value) throws InterpretException {
        return this.GT(value).OR(this.EQ(value));
    }
    public Value OR(Value value) {
        if (this.type == Symbol.TRUE || value.type == Symbol.TRUE) {
            return new Value(Symbol.TRUE);
        } else {
            return new Value(Symbol.FALSE);
        }
    }

    public Value LET(Value value) throws InterpretException {
        return NOT(this.GT(value));
    }

    public Value NEQ(Value value) throws InterpretException {
        return NOT(this.EQ(value));
    }

    public static Value NOT(Value value) throws InterpretException {
        if (value.type == Symbol.TRUE) {
            return new Value(Symbol.FALSE);
        } else if (value.type == Symbol.FALSE) {
            return new Value(Symbol.TRUE);
        } else if (value.type == Symbol.SINGLE_INT) {
            Value rv = new Value(Symbol.SINGLE_INT);
            rv.setIntValue(value.intValue * -1);
            return rv;
        } else if (value.type == Symbol.SINGLE_FLOAT) {
            Value rv = new Value(Symbol.SINGLE_FLOAT);
            rv.setFloatValue(value.floatValue * -1);
            return rv;
        }
        throw new InterpretException("负号使用非法");
    }
}
