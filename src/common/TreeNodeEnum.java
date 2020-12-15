package common;

/**
 * @Author: zlxx
 * @Date: 2020/12/6 21:59
 */
public enum TreeNodeEnum {
    /** 语句块入口 */
    NULL,

    /** 主函数入口 */
    MAIN_ENTER,
    /** if语句 */
    IF_STMT   ,
    /** for语句*/
    FOR_STMT  ,
    /** while语句*/
    WHILE_STMT,
    /** 声明变量语句*/
    DECLARE_STMT ,
    /** 字面量常数*/
    LITERAL,
    /** 操作符 */
    OP,
    /** 赋值语句*/
    ASSIGN_STMT,
    /** 变量*/
    VAR,
    /** 因式*/
    FACTOR,
    /** 运算表达式 */
    EXP,
    /** write */
    WRITE_STMT
}
