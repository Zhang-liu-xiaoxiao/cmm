package compile;/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:52
 */

import common.*;
import common.Compiler;

import java.util.LinkedList;

/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:52
 */
public class Generator {
    private  int mLevel;
    private  int mLine;
    private  LinkedList<FourCode> codes;
    private  SymbolTable symbolTable;
    private  Parser parser;


    public LinkedList<FourCode> getCodes() {
        return codes;
    }
    public SymbolTable getSymbolTable() {
        return symbolTable;
    }

    public Parser getParser() {
        return parser;
    }
    public void setParser(Parser parser) {
        this.parser = parser;
    }

    public Generator(Parser parser) {
        this.parser = parser;
        mLine = -1;//代码编号从0开始
        mLevel = 0;
        codes = new LinkedList<FourCode>();
        symbolTable = new SymbolTable();
    }

    public  LinkedList<FourCode> generateCode() throws InterpretException {
        LinkedList<TreeNode> nodeList = parser.getTreeNodeLinkedList();
        symbolTable.creatTable();
        for (TreeNode node : nodeList) {
            toGenerate(node);
        }
        symbolTable.deleteTable();
        return codes;
    }

    private void toGenerate(TreeNode node) throws InterpretException {

        while (true){
            switch (node.getNodeType()){
                case IF_STMT:
                    interpretIfStmt(node);
                    break;
                case WRITE_STMT:
                    codes.add(new FourCode(mLine,FourCodeEnum.WRITE.getValue(), null, null, interpretExp(node.getLeft())));
                    mLine++;
                    break;
                case WHILE_STMT:{
                    //跳转行是while语句的下一行
                    int nextLine = mLine+1;
                    FourCode nextJmp = new FourCode(mLine,FourCodeEnum.JMP.getValue(),
                            interpretExp(node.getLeft()), null, null);
                    nextJmp.setNumber(mLine);
                    codes.add(nextJmp);
                    //行数往下执行一行
                    //层级加一，因为跳进while体内
                    mLine++;
                    mLevel++;
                    //进入语句块
                    codes.add(new FourCode(mLine,FourCodeEnum.IN.getValue(), null, null, null));
                    mLine++;
                    mLevel++;
                    //解释while体内的语句
                    toGenerate(node.getMiddle());
                    //这里语句块内的语句解释完毕，可以删除那一层的变量了
                    symbolTable.deleteSymbolInLevel(mLevel);
                    mLevel--;
                    codes.add(new FourCode(mLine,FourCodeEnum.OUT.getValue(), null, null, null));
                    mLine++;
                    codes.add(new FourCode(mLine,FourCodeEnum.JMP.getValue(), null, null, nextLine + ""));
                    mLine++;
                    nextJmp.setForth(String.valueOf(mLine+1));
                    break;
                }
                case DECLARE_STMT:{

                    TreeNode var = node.getLeft();
                    String exp = null;
                    if (node.getMiddle()!=null){
                        exp = interpretExp(node.getMiddle());
                    }
                    if (var.getDataType() == Compiler.INT) {
                        codes.add(new FourCode(mLine,FourCodeEnum.INT.getValue(), exp, null, var.getValue()));
                        mLine++;
                        Symbol symbol = new Symbol(var.getValue(), Symbol.SINGLE_INT, mLevel);
                        symbolTable.addSymbol(symbol);
                    } else if (var.getDataType() == Compiler.FLOAT) {
                        codes.add(new FourCode(mLine,FourCodeEnum.FLOAT.getValue(), exp, null, var.getValue()));
                        mLine++;
                        Symbol symbol = new Symbol(var.getValue(), Symbol.SINGLE_FLOAT, mLevel);
                        symbolTable.addSymbol(symbol);
                    }
                    break;
                }
                case ASSIGN_STMT:{
                    String value = interpretExp(node.getMiddle());

                    TreeNode var = node.getLeft();
                    if (var.getLeft() == null) {
                        codes.add(new FourCode(mLine,FourCodeEnum.ASSIGN.getValue(), value, null, var.getValue()));
                    } else {
                        String index = interpretExp(var.getLeft());
                        codes.add(new FourCode(mLine,FourCodeEnum.ASSIGN.getValue(), value, null, var.getValue() + "[" + index + "]"));
                    }
                    mLine++;
                    break;

                }
                default:break;
            }
            symbolTable.clearTempNames();
            if (node.getNextNode() != null) {
                node = node.getNextNode();
            } else {
                break;
            }
        }
    }

    private String interpretExp(TreeNode node) throws InterpretException {
        if (node.getNodeType() == TreeNodeEnum.EXP) {
            switch (node.getDataType()) {
                case Compiler.LOGIC_EXP:
                    return interpretLogicExp(node);
                case Compiler.POLYNOMIAL:
                    return interpretPolynomial(node);
                case Compiler.TERM_EXP:
                    return interpretTermExp(node);
                default:
                    throw new InterpretException("复合表达式非法");
            }
        }
        //因子
        else if (node.getNodeType() == TreeNodeEnum.FACTOR) {
            if (node.getDataType() == Compiler.MINUS) {
                String temp = symbolTable.getTempSymbol().getName();
                codes.add(new FourCode(mLine,FourCodeEnum.MINUS.getValue(), interpretExp(node.getLeft()), null, temp));
                mLine++;
                return temp;
            } else {
                return interpretExp(node.getLeft());
            }
        }
            //在symboltable里寻找已经定义好的变量
        else if (node.getNodeType() == TreeNodeEnum.VAR) {
            if (symbolTable.getSymbolType(node.getValue()) == Symbol.SINGLE_INT || symbolTable.getSymbolType(node.getValue()) == Symbol.SINGLE_FLOAT) {
                return node.getValue();
            }
        }
        //字面常量
        else if (node.getNodeType() == TreeNodeEnum.LITERAL) {
            return node.getValue();
        }
        throw new InterpretException("表达式非法");
    }

    private String interpretTermExp(TreeNode node) throws InterpretException {
        String opcode = getOpcode(node.getMiddle().getDataType());
        String temp1 = symbolTable.getTempSymbol().getName();

        if (node.getLeft().getNodeType() == TreeNodeEnum.FACTOR) {
            codes.add(new FourCode(mLine,opcode,
                    interpretExp(node.getLeft()),
                    interpretExp(node.getRight().getLeft()),
                    temp1));
            mLine++;
        }
        //说明这个term操作符的右边还是一个term
        //具体表现形式为 a*b*c
        //运算要从左往右一个接一个
        //把值拼接起来
        else {
            codes.add(new FourCode(mLine,opcode, interpretExp(node.getLeft()), interpretExp(node.getRight().getLeft()), temp1));
            mLine++;
            node = node.getRight();
            String temp2 = null;
            while (node.getRight() != null && node.getRight().getNodeType() != TreeNodeEnum.FACTOR) {
                opcode = getOpcode(node.getMiddle().getDataType());
                temp2 = symbolTable.getTempSymbol().getName();
                codes.add(new FourCode(mLine,opcode, temp1, interpretExp(node.getRight().getLeft()), temp2));
                mLine++;
                node = node.getRight();
                temp1 = temp2;
            }
            opcode = getOpcode(node.getMiddle().getDataType());
            temp2 = symbolTable.getTempSymbol().getName();
            codes.add(new FourCode(mLine,opcode, temp1, interpretExp(node.getRight()), temp2));
            mLine++;
            temp1 = temp2;
        }
        return temp1;

    }

    private String getOpcode(int op) {
        if (op == Compiler.MUL) {
            return FourCodeEnum.MUL.getValue();
        } else {//Token.DIV
            return FourCodeEnum.DIV.getValue();
        }
    }

    private String interpretPolynomial(TreeNode node) throws InterpretException {
        String temp = symbolTable.getTempSymbol().getName();
        switch (node.getMiddle().getDataType()) {
            case Compiler.PLUS:
                codes.add(new FourCode(mLine,FourCodeEnum.PLUS.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            case Compiler.MINUS:
                codes.add(new FourCode(mLine,FourCodeEnum.MINUS.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            default:
                throw new InterpretException("算数运算非法");
        }
        mLine++;
        return temp;
    }

    private void interpretIfStmt(TreeNode node) throws InterpretException {
        if (node.getNodeType() == TreeNodeEnum.IF_STMT) {
            //条件跳转 jmp 条件  null 目标  条件为假时跳转
            FourCode falsejmp = new FourCode(mLine,FourCodeEnum.JMP.getValue(), interpretExp(node.getLeft()), null, null);
            codes.add(falsejmp);
            mLine++;
            codes.add(new FourCode(mLine,FourCodeEnum.IN.getValue(), null, null, null));
            mLine++;
            mLevel++;
            toGenerate(node.getMiddle());
            symbolTable.deleteSymbolInLevel(mLevel);
            mLevel--;
            codes.add(new FourCode(mLine,FourCodeEnum.OUT.getValue(), null, null, null));
            mLine++;
            if (node.getRight() != null) {
                FourCode outjump = new FourCode(mLine,FourCodeEnum.JMP.getValue(), null, null, null);
                codes.add(outjump);
                mLine++;
                falsejmp.setForth(String.valueOf(mLine + 1));
                codes.add(new FourCode(mLine,FourCodeEnum.IN.getValue(), null, null, null));
                mLine++;
                mLevel++;
                toGenerate(node.getRight());
                codes.add(new FourCode(mLine,FourCodeEnum.OUT.getValue(), null, null, null));
                mLine++;
                symbolTable.deleteSymbolInLevel(mLevel);
                mLevel--;
                outjump.setForth(String.valueOf(mLine + 1));
            } else {
                falsejmp.setForth(String.valueOf(mLine + 1));
            }
        }
    }

    private String interpretLogicExp(TreeNode node) throws InterpretException {
        String temp = symbolTable.getTempSymbol().getName();
        switch (node.getMiddle().getDataType()) {
            case Compiler.GT:
                codes.add(new FourCode(mLine,FourCodeEnum.GT.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            case Compiler.GET:
                codes.add(new FourCode(mLine,FourCodeEnum.GET.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            case Compiler.LT:
                codes.add(new FourCode(mLine,FourCodeEnum.LT.getValue(), interpretExp(node.getLeft()),:wq! interpretExp(node.getRight()), temp));
                break;
            case Compiler.LET:
                codes.add(new FourCode(mLine,FourCodeEnum.LET.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            case Compiler.EQ:
                codes.add(new FourCode(mLine,FourCodeEnum.EQ.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            case Compiler.NOTEQ:
                codes.add(new FourCode(mLine,FourCodeEnum.NEQ.getValue(), interpretExp(node.getLeft()), interpretExp(node.getRight()), temp));
                break;
            default:
                throw new InterpretException("逻辑比较非法");
        }
        mLine++;
        return temp;
    }
}
