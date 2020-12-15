package common;/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:54
 */

import compile.Parser;

import java.util.ArrayList;
import java.util.LinkedList;

/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:54
 */
public class SymbolTable {
    private static final String TEMP_PREFIX = "*temp";


    //创造的临时变量表
    private  LinkedList<Symbol> tempNames;
    private  ArrayList<Symbol> symbolList;

    public SymbolTable() {
    }

    public void creatTable() {
        symbolList = new ArrayList<Symbol>();
        tempNames = new LinkedList<Symbol>();
    }

    public void deleteTable() {
        if (symbolList != null) {
            symbolList.clear();
        }
        if (tempNames != null) {
            tempNames.clear();
        }
    }

    public void addSymbol(Symbol symbol) throws InterpretException {
        for (int i=0;i<symbolList.size();i++){
            Symbol symbol1 = symbolList.get(i);
            if (symbol.getName().equals(symbol1.getName())){
                if (symbol1.getLevel()<symbol.getLevel()){
                    symbolList.set(i,symbol);
                    symbol.setNext(symbol1);
                    return;
                }else {
                    throw new InterpretException("重复变量声明：" +symbol.getName());
                }
            }
        }
        symbolList.add(symbol);
    }

    public void deleteSymbolInLevel(int level){
        for (int i=0; i<symbolList.size(); i++) {
            if (symbolList.get(i).getLevel() == level) {
                if (symbolList.get(i).getNext()!=null){
                    symbolList.set(i, symbolList.get(i).getNext());
                }else {
                    symbolList.remove(i);
                }
            }
        }
    }

    public void setSymbolValue(String name, Value value) throws InterpretException {
        getSymbol(name).setValue(value);
    }

    public Value getSymbolValue(String name) throws InterpretException {
        return getSymbol(name).getValue();
    }

    private Symbol getSymbol(String name) throws InterpretException {
        for (Symbol s : symbolList) {
            if (s.getName().equals(name)) {
                return s;
            }
        }
        for (Symbol s : tempNames) {
            if (s.getName().equals(name)) {
                return s;
            }
        }
        if (name.startsWith(TEMP_PREFIX)) {
            Symbol s = new Symbol(name, Symbol.TEMP, -1);
            tempNames.add(s);
            return s;
        }
        throw new InterpretException("变量 <" + name + "> 不存在");
    }

    public int getSymbolType(String name) throws InterpretException {
        return getSymbol(name).getType();
    }
    /**
     * 获取一个没有使用的临时符号名
     * @return
     */
    public Symbol getTempSymbol() {
        String temp = null;
        for (int i = 1; ; i++) {
            temp = TEMP_PREFIX + i;
            boolean exist = false;
            for (Symbol s : tempNames) {
                if (s.getName().equals(temp)) {
                    exist = true;
                    break;
                }
            }
            for (Symbol s : symbolList) {
                if (s.getName().equals(temp)) {
                    exist = true;
                    break;
                }
            }
            if (exist) {
                continue;
            }
            Symbol s = new Symbol(temp, Symbol.TEMP, -1);
            tempNames.add(s);
            return s;
        }
    }

    /**
     * 清空临时符号名
     */
    public void clearTempNames() {
        tempNames.clear();
    }
}
