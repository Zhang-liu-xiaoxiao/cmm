package compile;/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:52
 */

import common.*;
import jdk.nashorn.internal.runtime.PropertyAccess;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:52
 */
public class Interpreter {
    private Generator generator;
    private SymbolTable symbolTable;
    private LinkedList<FourCode> codes;
    private int pc;
    private int level;
    private final List<String> results;

    public List<String> getResults() {
        return results;
    }


    public Interpreter(Generator generator) {
        this.generator = generator;
        symbolTable = generator.getSymbolTable();
        codes = generator.getCodes();
        pc = 0;
        level = 0;
        results = new ArrayList<>();
    }

    public void interpret() throws InterpretException {
        int length = codes.size();
        while (pc < length) {
            interpretFourCode(codes.get(pc));
        }
    }

    private void interpretFourCode(FourCode code) throws InterpretException {

        String type = code.getFirst();
        if (type.equals(FourCodeEnum.JMP.getValue())) {
            if (code.getSecond() == null || symbolTable.getSymbolValue(code.getSecond()).getType() == Symbol.FALSE) {//需要跳转
                pc = getValue(code.getForth()).getIntValue();
                return;
            }
        }
        if (type.equals(FourCodeEnum.WRITE.getValue())) {
            results.add(symbolTable.getSymbolValue(code.getForth()).toString());
        }
        if (type.equals(FourCodeEnum.IN.getValue())) {
            level++;
        }
        if (type.equals(FourCodeEnum.OUT.getValue())) {
            symbolTable.deleteSymbolInLevel(level);
            level--;
        }

        if (type.equals(FourCodeEnum.INT.getValue())) {
            int intValue = 0;
            if (code.getSecond() != null) {
                intValue = getInt(code.getSecond());
            }
            Symbol symbol = new Symbol(code.getForth(), Symbol.SINGLE_INT, level, intValue);
            symbolTable.addSymbol(symbol);

        }

        if (type.equals(FourCodeEnum.FLOAT.getValue())) {
            double floatValue = 0;
            if (code.getSecond() != null) {
                floatValue = getDouble(code.getSecond());
            }
            Symbol symbol = new Symbol(code.getForth(), Symbol.SINGLE_FLOAT, level, floatValue);
            symbolTable.addSymbol(symbol);
        }

        if (type.equals(FourCodeEnum.ASSIGN.getValue())) {
            Value value = getValue(code.getSecond());
            setValue(code.getForth(), value);
        }

        if (type.equals(FourCodeEnum.PLUS.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).PLUS(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.MINUS.getValue())) {
            if (code.getThird() != null) {
                setValue(code.getForth(), getValue(code.getSecond()).MINUS(getValue(code.getThird())));
            } else {
                setValue(code.getForth(), Value.NOT(getValue(code.getSecond())));
            }
        }
        if (type.equals(FourCodeEnum.MUL.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).MUL(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.DIV.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).DIV(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.GT.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).GT(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.LT.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).LT(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.EQ.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).EQ(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.GET.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).GET(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.LET.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).LET(getValue(code.getThird())));
        }
        if (type.equals(FourCodeEnum.NEQ.getValue())) {
            setValue(code.getForth(), getValue(code.getSecond()).NEQ(getValue(code.getThird())));
        }
        pc++;
    }

    private void setValue(String id, Value value) throws InterpretException {
        int type = symbolTable.getSymbolType(id);
        switch (type) {
            case Symbol.SINGLE_INT:
            case Symbol.SINGLE_FLOAT:
            {
                if (type == Symbol.SINGLE_FLOAT) {
                    symbolTable.setSymbolValue(id, value.toFloat());
                } else {
                    if (value.getType() == Symbol.SINGLE_FLOAT) {
                        throw new InterpretException("表达式" + id + "与变量类型不匹配");
                    } else {
                        symbolTable.setSymbolValue(id, value);
                    }
                }
                break;
            }
            case Symbol.TEMP:
                symbolTable.setSymbolValue(id, value);
                break;
            default:
                break;
        }
    }

    private double getDouble(String value) throws InterpretException {
        if (value.matches("^(-?\\d+)(\\.\\d+)?$")) {
            return Double.parseDouble(value);
        }
        Value valueInt = symbolTable.getSymbolValue(value);
        return valueInt.toFloat().getFloatValue();
    }

    private int getInt(String value) throws InterpretException {
        if (value.matches("^(-?\\d+)$")) {
            return Integer.parseInt(value);
        }
        Value valueInt = symbolTable.getSymbolValue(value);
        if (valueInt.getType() == Symbol.SINGLE_INT) {
            return valueInt.getIntValue();
        } else {
            throw new InterpretException("不是整数");
        }
    }

    private Value getValue(String id) throws InterpretException {
        if (id.matches("\\d*\\.\\d*")) {
            Value value = new Value(Symbol.SINGLE_FLOAT);
            value.setFloatValue(Double.parseDouble(id));
            return value;
        }
        if (id.matches("\\d+")) {
            Value value = new Value(Symbol.SINGLE_INT);
            value.setIntValue(Integer.parseInt(id));
            return value;
        }
        return symbolTable.getSymbolValue(id);
    }
}
