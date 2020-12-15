package compile;/**
 * @Author: zlxx
 * @Date: 2020/12/4 21:27
 */

import common.*;
import common.Compiler;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

/**
 * @Author: zlxx
 * @Date: 2020/12/4 21:27
 */
public class Parser {
    /** 语法分析的结果 */
    private  LexAnalyse lexAnalyse;
    /** 符号表 */
    private  List<Word> words;
    /** word 迭代器 */
    private  ListIterator<Word> iterator;
    /** 当前处理的word */
    private  Word currentWord;

    private  LinkedList<TreeNode> treeNodeLinkedList;

    public  LinkedList<TreeNode> getTreeNodeLinkedList() {
        return this.treeNodeLinkedList;
    }

    public Parser(LexAnalyse lexAnalyse) {
        this.lexAnalyse = lexAnalyse;
        this.words = lexAnalyse.wordList;
        this.treeNodeLinkedList = new LinkedList<>();
        this.iterator = words.listIterator();
        this.currentWord = null;
    }



    public StringBuilder grammarAnalyse() throws ParseException {
        StringBuilder stringBuilder = new StringBuilder();
        while (iterator.hasNext()) {
            TreeNode treeNode = null;
            if ((treeNode=parseStmt()) !=null) {
                treeNodeLinkedList.add(treeNode);
            }
        }
//        return treeNodeLinkedList;

        for (TreeNode treeNode:treeNodeLinkedList){
            printNodeWithIntent(stringBuilder,treeNode,0);
        }

        return stringBuilder;
    }

    private static void printNodeWithIntent(StringBuilder sb,TreeNode node,int indent){
        if (node.getNodeType() != TreeNodeEnum.NULL) {
            int t = indent;
            while (t>0) {
                sb.append("    ");
                t--;
            }
            sb.append(node.toString());
            sb.append(System.getProperty("line.separator"));
            switch (node.getNodeType()) {
                case IF_STMT:
                    if (node.getMiddle() != null) {
                        t = indent;
                        while (t>0) {
                            sb.append("    ");
                            t--;
                        }
                        sb.append("  THEN:");
                        sb.append(System.getProperty("line.separator"));
                        printNodeWithIntent(sb, node.getMiddle(), indent+1);
                    }
                    if (node.getRight() != null) {
                        t = indent;
                        while (t>0) {
                            sb.append("    ");
                            t--;
                        }
                        sb.append("  ELSE:");
                        sb.append(System.getProperty("line.separator"));
                        printNodeWithIntent(sb, node.getRight(), indent+1);
                    }
                    break;
                case WHILE_STMT:
                    if (node.getMiddle() != null) {
                        printNodeWithIntent(sb, node.getMiddle(), indent+1);
                    }
                    break;
                default:
                    break;
            }
        }
        if (node.getNextNode() != null) {
            printNodeWithIntent(sb, node.getNextNode(), indent);
        }
    }

    private TreeNode parseStmt() throws ParseException {
        switch (getNextWordType()) {
            case Compiler.IF:
                return parseIfStmt();
            case Compiler.WHILE:
                return parseWhileStmt();
            case Compiler.VOID:
                return parseMainStmt();
            case Compiler.INT:
            case Compiler.FLOAT:
                return parseDeclareStmt();
            case Compiler.WRITE:
                return parseWriteStmt();
            case Compiler.LBRACE:
                return parseBlockStmt();
            case Compiler.ID:
                return parseAssignStmt();
            case Compiler.FOR:
                return parseForStmt();
            default:
                throw new ParseException("not illegal word ,line" + currentWord.getLine()
                +"current word type and value "+ currentWord.getType() + currentWord.getValue());
        }
    }

    private TreeNode parseForStmt() {
        return null;
    }

    /**
     * 处理赋值的语句
     * 赋值语句是 标识符 = exp
     * 所以按顺序处理就好了
     * @return
     * @throws ParseException
     */
    private TreeNode parseAssignStmt() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.ASSIGN_STMT);
        treeNode.setLeft(variableName());
        consumeNextWord(Compiler.ASSIGN);
        treeNode.setMiddle(parseExp());
        consumeNextWord(Compiler.SEMICOLONS);
        return treeNode;
    }

    /**
     * 处理块语句
     * 这个过程要存储语句块深度
     * 存储的方法是用treenode的next指向下一层语句就好
     * 他本身不要存属性，单独用来表示层级关系
     * @return
     */
    private TreeNode parseBlockStmt() throws ParseException {
        TreeNode node = new TreeNode(TreeNodeEnum.NULL);
        TreeNode header = node;
        TreeNode temp = null;
        consumeNextWord(Compiler.LBRACE);

        //允许语句块中没有语句
        //循环处理，用一个结点连接所有的块级语句
        while (getNextWordType() != Compiler.RBRACE) {
            temp = parseStmt();
            node.setNextNode(temp);
            node = temp;
        }
        consumeNextWord(Compiler.RBRACE);
        return header;
    }

    private TreeNode parseDeclareStmt() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.DECLARE_STMT);
        TreeNode varNode = new TreeNode(TreeNodeEnum.VAR);
        // 声明阶段，声明一个变量
        if (checkNextTokenType(Compiler.INT,Compiler.FLOAT)){
            currentWord = iterator.next();
            int type;
            if ((type = currentWord.getType()) == Compiler.INT){
                varNode.setDataType(Compiler.INT);
            }else if(type == Compiler.FLOAT){
                varNode.setDataType(Compiler.FLOAT);
            }
        }else {
            throw new ParseException("line "+getNextWordLine() + "should be int or float");
        }
        //声明变量的标识符
        if(checkNextTokenType(Compiler.ID)){
            currentWord = iterator.next();
            varNode.setValue(currentWord.getValue());
        }else {
            throw new ParseException("line "+getNextWordLine() + "should be a identifier");
        }

        //可能存在的赋值阶段，也有可能只声明不赋值所以要判断
        if (checkNextTokenType(Compiler.ASSIGN)){
            consumeNextWord(Compiler.ASSIGN);
            treeNode.setMiddle(parseExp());
        }
        consumeNextWord(Compiler.SEMICOLONS);
        treeNode.setLeft(varNode);
        return treeNode;
    }

    private TreeNode parseMainStmt() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.MAIN_ENTER);

        consumeNextWord(Compiler.VOID);
        consumeNextWord(Compiler.MAIN);
        consumeNextWord(Compiler.LPARENT);
        consumeNextWord(Compiler.RPARENT);

        return null;
    }

    /**
     * 要处理 while语句
     * 先把while( 处理掉
     * 再处理 exp
     *
     * @return 结点
     * @throws ParseException
     */
    private TreeNode parseWhileStmt() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.WHILE_STMT);

        consumeNextWord(Compiler.WHILE);
        consumeNextWord(Compiler.LPARENT);
        treeNode.setLeft(parseExp());
        consumeNextWord(Compiler.RPARENT);
        treeNode.setMiddle(parseStmt());
        return treeNode;
    }

    private  TreeNode parseWriteStmt() throws ParseException {
        TreeNode node = new TreeNode(TreeNodeEnum.WRITE_STMT);
        consumeNextWord(Compiler.WRITE);
        node.setLeft(parseExp());
        consumeNextWord(Compiler.SEMICOLONS);
        return node;
    }
    /**
     * 表达式
     * 表达式都是 多项式 运算符 多项式
     * 所以要按顺序处理
     * @throws ParseException
     */
    private  TreeNode parseExp() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.EXP);
        treeNode.setDataType(Compiler.LOGIC_EXP);
        //递归解析第一个多项式
        TreeNode node1 = parsePolynomial();
        //如果之后的符号不是运算符，说明表达式只有一个多项式
        //直接返回多项式结点
        //否则反汇表达式
        if (checkNextTokenType(Compiler.EQ,Compiler.GET,Compiler.GT,Compiler.LET,Compiler.LT,Compiler.NOTEQ)){
            treeNode.setLeft(node1);
            //获得运算符结点，放在中间
            treeNode.setMiddle(parseLogicalOp());
            //处理下一个多项式
            treeNode.setRight(parsePolynomial());
            return treeNode;
        }
        //到了这说明只有一个多项式，直接返回他就好
        return node1;
    }

    /**
     * parse到了运算符
     * 要返回下一个word代表的运算符
     * 用treenode封装
     * @throws ParseException
     */
    private  TreeNode parseLogicalOp() throws ParseException {
        if (iterator.hasNext()) {
            currentWord = iterator.next();
            int type = currentWord.getType();
            if (type == Compiler.EQ
                    || type == Compiler.GET
                    || type == Compiler.GT
                    || type == Compiler.LET
                    || type == Compiler.LT
                    || type == Compiler.NOTEQ) {
                TreeNode node = new TreeNode(TreeNodeEnum.OP);
                node.setDataType(type);
                return node;
            }
        }
        throw new ParseException("line " + getNextWordLine() + " : next token should be logical operator");
    }

    /**
     * 处理多项式
     * 多项式可能由多个项组成
     * 多项式由加减连接起来的
     * 乘除连接到一起的是一个项
     * @return treenode
     */
    private  TreeNode parsePolynomial() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.EXP);
        treeNode.setDataType(Compiler.POLYNOMIAL);
        TreeNode term = parseTerm();
        if (checkNextTokenType(Compiler.PLUS,Compiler.MINUS))
        {
            treeNode.setLeft(term);
            treeNode.setMiddle(addtiveOp());
            treeNode.setRight(parseTerm());
            return treeNode;
        }
        return term;

    }

    private  TreeNode addtiveOp() throws ParseException {
        if (iterator.hasNext()) {
            currentWord = iterator.next();
            int type = currentWord.getType();
            if (type == Compiler.PLUS || type == Compiler.MINUS) {
                TreeNode node = new TreeNode(TreeNodeEnum.OP);
                node.setDataType(type);
                return node;
            }
        }
        throw new ParseException("line " + getNextWordLine() + " : next token should be addtive operator");
    }
    /**
     * 解析一个项
     * 像可能是因式和因式的运算
     * 也有可能就是一个因式
     * @return
     * @throws ParseException
     */

    private TreeNode parseTerm() throws ParseException {
        TreeNode node = new TreeNode(TreeNodeEnum.EXP);
        node.setDataType(Compiler.TERM_EXP);
        TreeNode leftNode = parseFactor();
        if (checkNextTokenType(Compiler.MUL, Compiler.DIV)) {
            node.setLeft(leftNode);
            node.setMiddle(multiplyOp());
            node.setRight(parseTerm());
            return node;
        }
        return leftNode;
    }

    /**
     * 因子
     * 因子也可能是一个表达式
     * 或者是变量
     * 再或者是字面常量
     * @throws ParseException
     */
    private  TreeNode parseFactor() throws  ParseException {
        if (iterator.hasNext()) {
            TreeNode expNode = new TreeNode(TreeNodeEnum.FACTOR);
            switch (getNextWordType()) {
                //如果是字面常量类型，直接把值放入左节点
                case Compiler.LITERAL_INT:
                case Compiler.LITERAL_FLOAT:
                    expNode.setLeft(parseLiteral());
                    break;
                //如果有括号，把里面当作表达式处理
                //所以CMM中if表达式语句需要加括号
                case Compiler.LPARENT:
                    consumeNextWord(Compiler.LPARENT);
                    expNode = parseExp();
                    consumeNextWord(Compiler.RPARENT);
                    break;
                //如果是运算符的话
                case Compiler.MINUS:
                    expNode.setDataType(Compiler.MINUS);
                    currentWord = iterator.next();
                    expNode.setLeft(parseTerm());
                    break;
                case Compiler.PLUS:
                    currentWord = iterator.next();
                    expNode.setLeft(parseTerm());
                    break;
                default:
                    //返回的不是expNode
                    //返回的是一个变量
                    return variableName();
            }
            return expNode;
        }
        throw new ParseException("line " + getNextWordLine() + " : next token should be factor");
    }

    /**
     * 解析一个变量
     * 比较好理解
     * @return
     * @throws ParseException
     */
    private TreeNode variableName() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.VAR);
        if (checkNextTokenType(Compiler.ID)){
            currentWord = iterator.next();
            treeNode.setValue(currentWord.getValue());
        }else {
            throw new ParseException("line " + getNextWordLine() + "should be a identifier");
        }
        return treeNode;
    }

    private TreeNode parseLiteral() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.LITERAL);
        if (checkNextTokenType(Compiler.LITERAL_INT,Compiler.LITERAL_FLOAT)){
            currentWord = iterator.next();
            treeNode.setValue(currentWord.getValue());
        }else {
            throw new ParseException("line " + getNextWordLine() + "should be a literal ");
        }
        return treeNode;
    }

    /**
     * 乘除运算符
     * @throws ParseException
     */
    private TreeNode multiplyOp() throws ParseException {
        if (iterator.hasNext()) {
            currentWord = iterator.next();
            int type = currentWord.getType();
            if (type == Compiler.MUL || type == Compiler.DIV) {
                TreeNode node = new TreeNode(TreeNodeEnum.OP);
                node.setDataType(type);
                return node;
            }
        }
        throw new ParseException("line " + getNextWordLine() + " : next token should be multiple operator");
    }

    private boolean checkNextTokenType(int... type) {
        if (iterator.hasNext()) {
            int nextType = iterator.next().getType();
            iterator.previous();
            for (int each : type) {
                if (nextType == each) {
                    return true;
                }
            }
        }
        return false;
    }

    private TreeNode parseIfStmt() throws ParseException {
        TreeNode treeNode = new TreeNode(TreeNodeEnum.IF_STMT);
        //消耗不存储的符号
        consumeNextWord(Compiler.IF);
        consumeNextWord(Compiler.LPARENT);
        //把判断表达式放入左结点
        treeNode.setLeft(parseExp());
        consumeNextWord(Compiler.RPARENT);
        //把if语句放入中间结点
        treeNode.setMiddle(parseStmt());

        //如果if后面跟了else语句
        //处理就好了
        if (checkNextTokenType(Compiler.ELSE))
        {
            consumeNextWord(Compiler.ELSE);
            treeNode.setRight(parseStmt());
        }
        return treeNode;
    }


    int getNextWordLine()
    {
        if (iterator.hasNext()) {
            int lineNo = iterator.next().getLine();
            iterator.previous();
            return lineNo;
        }
        return -1;
    }
    int getNextWordType()
    {
        if (iterator.hasNext()) {
            int type = iterator.next().getType();
            iterator.previous();
            return type;
        }
        return -1;
    }

    /**
     * 消耗掉下一个token,要求必须是type类型,消耗之后currentToken值将停在最后消耗的token上
     * @param type
     * @throws ParseException 消耗失败则抛出
     */
    private void consumeNextWord(int type) throws ParseException {
        if (iterator.hasNext()) {
            currentWord = iterator.next();
            if (currentWord.getType() == type) {
                return;
            }
        }
        throw new ParseException("line " + getNextWordLine() + " : next word wrong should be "+ type +
                Compiler.stringList.get(type));
    }
}
