package compile;
/**
 * @Author: zlxx
 * @Date: 2020/12/4 21:22
 */

import common.Compiler;
import common.LexError;
import common.Word;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author: zlxx
 * @Date: 2020/12/4 21:22
 */
public class LexAnalyse {
    public  List<Word> wordList;
    private final List<LexError> errorList;
    private final String text;
    private boolean isMultiLineNote;
    private int errorCount;
    private int wordCount;
    private boolean lexHasError = false;
    public LexAnalyse(String text) {
        this.text = text;
        wordList = new ArrayList<>();
        errorList = new ArrayList<>();
        isMultiLineNote = false;
        wordCount = 0;
        errorCount = 0;
        lexAnalyse();
    }

    public void lexAnalyse()
    {
        String[] lines = this.text.split("\n");
        int lineNumber = 1;
        for (String line : lines) {
            analyse(line.trim(), lineNumber);
            lineNumber++;
        }
    }

    public void analyse(String wordInOneLine,int lineNumber)
    {
        int index=0;
        int oneWordStart;
        int length = wordInOneLine.length();
        Word word = null;
        while (index<length){
            if (!isMultiLineNote){
            char c = wordInOneLine.charAt(index);
                //dfa process
                //is identifier or reserved word
                if (c=='_'|| Compiler.isLetter(c)){
                    oneWordStart = index;
                    index++;

                    //separate every lex
                    while ((index<length)
                    && !Compiler.isBoundarySign(wordInOneLine.substring(index,index+1))
                    && !Compiler.isOperator(wordInOneLine.substring(index,index+1))
                    && isNotSpace(wordInOneLine.charAt(index))){
                        index++;
                    }
                    String oneWord= wordInOneLine.substring(oneWordStart, index);
                    wordCount++;
                    word = new Word();
                    word.setId(wordCount);
                    word.setLine(lineNumber);
                    word.setValue(oneWord);
                    wordList.add(word);
                    if (Compiler.isReserved(oneWord)) {
                        word.setType(Compiler.getTokenIndexFromString(oneWord));
                    }else if (Compiler.isID(oneWord)){
                        word.setType(Compiler.ID);
                    }else {//must be wrong,because in this if,the word must be identifier or reserved word
                        errorCount++;
                        word.setType(Compiler.UNDEFINED);
                        word.setFlag(false);
                        LexError lexError = new LexError(word, word.getLine(), "illegal identifier", errorCount);
                        wordList.remove(word);
                        wordCount--;//deserve thought to do or not
                        errorList.add(lexError);
                        lexHasError = true;
                    }
                    index--; //return pointer,like what i was taught in class
                }
                //in this case,maybe '=',or '=='
                else if(c == '='){
                    index++;
                    //"=="case
                    if (wordInOneLine.charAt(index) == '='){
                        wordCount++;
                        word = new Word(wordCount,"==",Compiler.EQ,lineNumber,true);
                    }else {
                        wordCount++;
                        word =new Word(wordCount,"=",Compiler.ASSIGN,lineNumber,true);
                        index--;//same as before,pointer return
                    }
                    wordList.add(word);
                }
                //maybe "++" ,'+'
                else if (c == '+'){
                    index++;
                    //"=="case
                    if (wordInOneLine.charAt(index) == '+'){
                        wordCount++;
                        word = new Word(wordCount,"++",Compiler.SELFPLUS,lineNumber,true);
                    }else {
                        wordCount++;
                        word =new Word(wordCount,"+",Compiler.PLUS,lineNumber,true);
                        index--;//same as before,pointer return
                    }
                    wordList.add(word);
                }
                //maybe "--" ,'-'
                else if (c == '-'){
                    index++;
                    //"=="case
                    if (wordInOneLine.charAt(index) == '-'){
                        wordCount++;
                        word = new Word(wordCount,"--",Compiler.SELFMINUS,lineNumber,true);
                    }else {
                        wordCount++;
                        word =new Word(wordCount,"-",Compiler.MINUS,lineNumber,true);
                        index--;//same as before,pointer return
                    }
                    wordList.add(word);
                }
                //maybe "&" ,'&&'
                else if (c == '&'){
                    index++;
                    //"=="case
                    if (wordInOneLine.charAt(index) == '&'){
                        wordCount++;
                        word = new Word(wordCount,"&&",Compiler.LOGICAND,lineNumber,true);
                    }else {
                        wordCount++;
                        word =new Word(wordCount,"&",Compiler.BITAND,lineNumber,true);
                        index--;//same as before,pointer return
                    }
                    wordList.add(word);
                }
                //maybe "||" ,'|'
                else if (c == '|'){
                    index++;
                    //"=="case
                    if (wordInOneLine.charAt(index) == '|'){
                        wordCount++;
                        word = new Word(wordCount,"||",Compiler.LOGICOR,lineNumber,true);
                    }else {
                        wordCount++;
                        word =new Word(wordCount,"|",Compiler.BITOR,lineNumber,true);
                        index--;//same as before,pointer return
                    }
                    wordList.add(word);
                }
                //maybe "!" ,'='
                else if (c == '!'){
                    index++;
                    //"=="case
                    if (wordInOneLine.charAt(index) == '='){
                        wordCount++;
                        word = new Word(wordCount,"!=",Compiler.NOTEQ,lineNumber,true);
                    }else {
                        wordCount++;
                        word =new Word(wordCount,"!",Compiler.BITREVERSE,lineNumber,true);
                        index--;//same as before,pointer return
                    }
                    wordList.add(word);
                }
                //judge if Integer or float
                else if (Compiler.isDigit(c))
                {
                    boolean alreadyDot = false;
                    boolean illegal = false;
                    oneWordStart = index;
                    index++;
                    while (Compiler.isDigit(wordInOneLine.charAt(index))
                    || wordInOneLine.charAt(index) == '.'){
                        if (wordInOneLine.charAt(index) == '.')
                        {
                            if (alreadyDot){
                                illegal = true;
                            }
                            alreadyDot = true;
                        }
                            index++;
                    }
                    wordCount++;
                    word= new Word();
                    word.setId(wordCount);
                    word.setValue(wordInOneLine.substring(oneWordStart,index));
                    word.setLine(lineNumber);
                    if (illegal){
                        word.setType(Compiler.UNDEFINED);
                        word.setFlag(false);
                        errorCount++;
                        errorList.add(new LexError(word, lineNumber, "wrong number!", errorCount));
                        lexHasError = true;

                    }else {
                        word.setFlag(true);
                        if (alreadyDot){
                            word.setType(Compiler.LITERAL_FLOAT);
                        }else {
                            word.setType(Compiler.LITERAL_INT);
                        }
                    }
                    wordList.add(word);
                    index--;
                }
                //handle '/',"//","/*"case
                else if (c == '/')
                {
                    index++;
                    if (wordInOneLine.charAt(index) == '/'){
                        break;//jump out this line, go to next line
                    }else if(wordInOneLine.charAt(index) == '*') {
                        isMultiLineNote = true;
                    }else {
                        wordCount++;
                        wordList.add(new Word(wordCount,"/",Compiler.DIV,lineNumber,true));
                    }
                    index--;
                }
                else if ( "'".equals(String.valueOf(c))){
                    if (!"'".equals(String.valueOf(wordInOneLine.charAt(index + 2)))){
                        wordCount++;
                        word = new Word(wordCount,"'",Compiler.UNDEFINED,lineNumber,false);
                        errorList.add(new LexError(word, lineNumber, "错误出现的单引号", ++errorCount));
                        lexHasError = true;

                    }else {
                        word = new Word(++wordCount,wordInOneLine.substring(index,index+3),Compiler.LITERAL_CHAR,lineNumber,false);
                        wordList.add(word);
                        index+=2;
                    }
                }
                //when come across this situation,it's not identifier number character
                //or some operator,mostly some space , separator
                else {
                    switch (c){
                        case ' ':
                        case '\t':
                        case '\r':
                        case '\n':
                            break;// skip 'white' character

                        case '[':
                        case ']':
                        case '(':
                        case ')':
                        case '{':
                        case '}':
                        case ';':
                        //this '*' should be paid attention
                        //it concerns about multiline note on or off
                        case '*':
                        case '%':
                        case '>':
                        case '<':
                        case '#':
                        case ',':
                            int tokenIndex = Compiler.getTokenIndexFromString(String.valueOf(c));
                            word = new Word();
                            wordList.add(word);
                            word.setId(++wordCount);
                            word.setLine(lineNumber);
                            word.setValue(String.valueOf(c));
                            word.setType(tokenIndex);
                            break;
                        default:
                            word = new Word(++wordCount,String.valueOf(c),Compiler.UNDEFINED,lineNumber,false);
                            errorList.add(new LexError(word,lineNumber,"不确定的标识符",++errorCount));
                            lexHasError = true;

                    }
                }
            }else {
                int i = wordInOneLine.indexOf("*/");
                if (i!=-1) {
                    isMultiLineNote = false;
                    index = i+2;
                    continue;
                }else {
                    break;
                }
            }
            index++;
        }
    }

    public boolean isNotSpace(char c){
       return
          (c!='\n') &&(c!='\t') &&(c!='\r') &&(c!=' ');
    }

    public String getWordList() throws IOException {
        File file = new File("./output/");
        if (!file.exists()) {
            file.mkdirs();
            file.createNewFile();// 如果这个文件不存在就创建它
        }
        String path = file.getAbsolutePath();
        FileOutputStream fos = new FileOutputStream(path + "/wordList.txt");
        BufferedOutputStream bos = new BufferedOutputStream(fos);
        OutputStreamWriter osw1 = new OutputStreamWriter(bos, "utf-8");
        PrintWriter pw1 = new PrintWriter(osw1);
        pw1.printf("%12s %12s %12s %12s %12s","单词序号","单词的值","单词类型","单词所在行","单词是否合法");
        pw1.println();


        wordList.stream()
                .forEach((word) ->{
                    pw1.printf("%20s %30s %20s %30s %20s",
                            String.valueOf(word.getId()),
                            String.valueOf(word.getValue()),
                            String.valueOf(word.getType()),
                            String.valueOf(word.getLine()),
                            String.valueOf(word.isLegal()));
                    pw1.println();

                });
        if (lexHasError) {
            LexError error;
            pw1.println("错误信息如下：");

            pw1.println("错误序号\t\t\t错误信息\t\t\t错误所在行 \t\t\t错误单词");
            for (int i = 0; i < errorList.size(); i++) {
                error = errorList.get(i);
                pw1.println(error.getErrorCount() + "\t\t\t" + error.getInfo() + "\t\t\t" + error.getLineNumber()
                        + "\t\t\t" + error.getWord().getValue());
            }
        } else {
            pw1.println("词法分析通过：");
        }
        pw1.close();
        return path + "/wordList.txt";
    }
}
