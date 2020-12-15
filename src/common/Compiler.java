package common;/**
 * @Author: zlxx
 * @Date: 2020/12/4 22:28
 */

import org.junit.Test;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @Author: zlxx
 * @Date: 2020/12/4 22:28
 */
public class Compiler {
    static final public List<String> SEPARATE_CHARACTER;
    static final public List<String> RESERVE_WORD;
    static final public List<String> OPERATE_CHARACTER;
    public final static String KEY = "keyValue";
    public final static String OPERATOR = "operator";
    public final static String INT_CONST = "intConst";
    public final static String FLOAT_CONST = "floatConst";

    public final static String CHAR_CONST = "charConst";
    public final static String BOOL_CONST = "booleanConst";
    public final static String IDENTIFIER = "identifier";
    public final static String BOUNDARY = "boundary";
    public static List<String> stringList;

    /** 初始化时默认Token没有类型 */
    public static final int NULL = 0;
    /** if */
    public static final int IF = 1;
    /** else */
    public static final int ELSE = 2;
    /** while */
    public static final int WHILE = 3;
    /** for */
    public static final int FOR = 4;
    /** int */
    public static final int INT = 5;
    /** main */
    public static final int MAIN = 6;
    /** void */
    public static final int VOID = 7;
    /** + */
    public static final int PLUS = 8;
    /** - */
    public static final int MINUS = 9;
    /** * */
    public static final int MUL = 10;
    /** / */
    public static final int DIV = 11;
    /** = */
    public static final int ASSIGN = 12;
    /** < */
    public static final int LT = 13;
    /** ||  */
    public static final int LOGICOR = 14;
    /** &&  */
    public static final int LOGICAND = 15;
    /** == */
    public static final int EQ = 16;
    /** ( */
    public static final int LPARENT = 17;
    /** ) */
    public static final int RPARENT = 18;
    /** { */
    public static final int LBRACE = 19;
    /** } */
    public static final int RBRACE = 20;
    /** [ */
    public static final int LBRACKET = 21;
    /** ] */
    public static final int RBRACKET = 22;
    /** <= */
    public static final int LET = 23;
    /** > */
    public static final int GT = 24;
    /** >= */
    public static final int GET = 25;
    /** 标识符,由数字,字母或下划线组成,第一个字符不能是数字 */
    public static final int ID = 26;
    /** int型字面值 */
    public static final int LITERAL_INT = 27;
    /** float型字面值 */
    public static final int LITERAL_FLOAT = 28;
    /** 逻辑表达式 */
    public static final int LOGIC_EXP = 29;
    /** 多项式 */
    public static final int POLYNOMIAL = 30;
    /** 项 */
    public static final int TERM_EXP = 31;

    /** error */
    public final static int UNDEFINED = 32;

    /** ++ */
    public final static int SELFPLUS = 33;

    /** -- */
    public final static int SELFMINUS = 34;
    /** & */
    public final static int BITAND = 35;
    /** | */
    public final static int BITOR = 36;
    /** ! */
    public final static int BITREVERSE = 37;
    /** != */
    public final static int NOTEQ = 38;
    /** char 字面类型 */
    public final static int LITERAL_CHAR = 39;
    /** # */
    public final static int END = 40;
    /** ; */
    public final static int SEMICOLONS = 41;
    /** , */
    public final static int COMMA = 42;
    /** float */
    public final static int FLOAT = 43;
    /** write */
    public final static int WRITE = 44;

    private static Map<Integer,String> map = new HashMap<>();

    static {
        SEPARATE_CHARACTER = Arrays.asList("{","}",";",",","(",")");
        RESERVE_WORD = Arrays.asList("int","main","void","float","double","return",
                "if","else","while","for","write");
        OPERATE_CHARACTER = Arrays.asList("+","-","++","--","&","|","!","==","*",">","<","=");

        stringList = new ArrayList<>(Arrays.asList("null", "if", "else", "while", "for", "int", "main", "void", "+", "-",
                "*", "/", "=", "<", "||", "&&", "==", "(", ")", "{", "}", "[", "]", "<=", ">", ">=", "ID",
                "LITERAL_INT", "LITERAL_FLOAT", "LOGIC_EXP", "POLYNOMIAL",
                "TERM_EXP", "UNDEFINED","++","--","&","|","!","!=","LITERAL_CHAR","#",";",",","float","write"));

        }






    public static boolean isReserved(String word) {
        return RESERVE_WORD.contains(word);
    }
    /**
     * 判断单词是否为int常量
     *
     * @param word
     * @return
     */
    public static boolean isInteger(String word) {
        for (int i = 0; i < word.length(); i++) {
            if (Character.isDigit(word.charAt(i))) {
                continue;
            } else {
                return false;
            }
        }

        return true;
    }

    /**
     * 判断单词是否为char常量
     *
     * @param word
     * @return
     */
    public static boolean isChar(String word) {
        boolean flag = false;
        int i = 0;
        char temp = word.charAt(i);
        if (temp == '\'') {
            for (i = 1; i < word.length(); i++) {
                temp = word.charAt(i);
                if (temp >255 ) {
                    break;
                }
            }
            if (i + 1 == word.length() && word.charAt(i) == '\'') {
                flag = true;
            }
        } else {
            return false;
        }

        return flag;
    }

    /**
     * 判断字符是否为字母
     *
     * @param ch
     * @return is or no
     */
    public static boolean isLetter(char ch) {
        return  (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z'));
    }

    /**
     * 判断单词是否为合法标识符
     *
     * @param word
     * @return
     */
    public static boolean isID(String word) {
        boolean flag = false;
        int i = 0;
        if (isReserved(word)) {
            return false;
        }
        char temp = word.charAt(i);
        if (isLetter(temp) || temp == '_') {
            for (i = 1; i < word.length(); i++) {
                temp = word.charAt(i);
                if (!(isLetter(temp) || temp == '_' || isDigit(temp))) {
                    break;
                }
            }
            if (i >= word.length()) {
                flag = true;
            }
        } else {
            return false;
        }
        return flag;
    }
    public static int getTokenIndexFromString(String s){
        return stringList.indexOf(s);
    }

    public static boolean isDigit(char ch) {
        return  ('0' <= ch && ch <= '9');
    }
    public static boolean isBoundarySign(String word) {
        return SEPARATE_CHARACTER.contains(word);
    }

    public static boolean isOperator(String word) {
        return OPERATE_CHARACTER.contains(word);
    }
}
