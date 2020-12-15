package common;

/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:57
 */
public enum FourCodeEnum {

    /**
     * jmp 条件  null 目标  是条件为假时跳转到目标
     * jmp null null 目标  无条件跳转到目标, 超过语句数,则程序结束
     * assign 元素 null 目标
     * int/float null 元素个数/null 变量名
     * in null null null 进入语句块
     * out null null null 出语句块
     * assign 值 null 目标
     * +
     * -
     * *
     * /
     */

    JMP("jmp"),
    IN("in"),
    OUT("out"),
    INT("int"),
    FLOAT("float"),
    ASSIGN("assign"),
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),
    GT(">"),
    LT("<"),
    GET(">="),
    LET("<="),
    EQ("=="),
    NEQ("!="),
    WRITE("write");

    private final String value;
    FourCodeEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
