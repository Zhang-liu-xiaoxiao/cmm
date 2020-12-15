package common;
/**
 * @Author: zlxx
 * @Date: 2020/12/6 18:30
 */

/**
 * @Author: zlxx
 * @Date: 2020/12/6 18:30
 * 语法分析树的结点，为以后的语义分析做好准备
 */

public class TreeNode {

    private String value;
    /** 左子节点 */
    private TreeNode Left;
    /** 中间节点 */
    private TreeNode Middle;
    /** 右节点 */
    private TreeNode Right;
    /** 下一层括号内的节点链表，用于语义分析 */
    private TreeNode nextNode;
    /** 每个结点都需要，指明这个结点类型 */
    /** 部分结点需要，比如操作符，字面常量，目的是复用以及存储额外信息*/
    private int dataType;

    private TreeNodeEnum nodeType;

    public TreeNode() {
    }
    public TreeNode(TreeNodeEnum nodeType) {
        this.nodeType = nodeType;
    }
    public String getValue() {
        return value;
    }
    public void setValue(String value) {
        this.value = value;
    }
    public TreeNode getLeft() {
        return Left;
    }
    public void setLeft(TreeNode left) {
        this.Left = left;
    }
    public TreeNode getMiddle() {
        return Middle;
    }
    public void setMiddle(TreeNode middle) {
        this.Middle = middle;
    }
    public TreeNode getRight() {
        return Right;
    }
    public void setRight(TreeNode right) {
        this.Right = right;
    }
    public TreeNode getNextNode() {
        return nextNode;
    }
    public void setNextNode(TreeNode nextNode) {
        this.nextNode = nextNode;
    }
    public TreeNodeEnum getNodeType() {
        return nodeType;
    }
    public void setNodeType(TreeNodeEnum nodeType) {
        this.nodeType = nodeType;
    }
    public int getDataType() {
        return dataType;
    }
    public void setDataType(int dateType) {
        this.dataType = dateType;
    }
    @Override
    public String toString() {
        switch (this.nodeType) {
            case IF_STMT:
                return "IF_STMT";
            case WHILE_STMT:
                return "WHILE_STMT";
            case DECLARE_STMT:
                return "DECLARE_STMT";
            case ASSIGN_STMT:
                return "ASSIGN_STMT";
            case EXP:
                return Compiler.stringList.get(this.getDataType());
            case VAR:
                return this.value;// return "VAR";
            case FACTOR:
                return "factor";
            case LITERAL:
                return "literal";
            case OP:
                Word word = new Word();
                word.setType(this.dataType);
                return word.toString();// return "OP";
            case NULL:
                return "{";// return "BLOCK HEADER";
            case MAIN_ENTER:
                return "MAIN";// return "BLOCK HEADER";
            case WRITE_STMT:
                return "WRITE";
            default:
                return "UNKNOWN";
        }
    }
}
