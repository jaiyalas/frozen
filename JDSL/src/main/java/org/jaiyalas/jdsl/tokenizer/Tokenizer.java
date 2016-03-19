package org.jaiyalas.jdsl.tokenizer;

import java.lang.StringBuffer;
import java.util.LinkedList;

import org.jaiyalas.jdsl.tokenizer.Token;
import org.jaiyalas.jdsl.tokenizer.TokenType;

public class Tokenizer{

    private StringBuffer temp;
    private String rawCtx;
    private LinkedList<Token> tokens;

    public Tokenizer(String _rawCtx){
	this.resetTemp();
	this.rawCtx = _rawCtx;
	this.tokens = new LinkedList<Token>();
	boolean quoteMode = false;
	for(int i = 0 ; i < this.rawCtx.length() ; i++){
	    char c = this.rawCtx.charAt(i);
	    switch(c){
	    case '\n':
	    case '{':
	    case '}':
	    case ':':
	    case '|':
	    case ',':
		this.tokens.add(Token.getStaticTk(c));
		break;
	    case '"':
		if(quoteMode){genQTkFromTemp();}
		quoteMode = !quoteMode;
		resetTemp();
		break;
	    case ' ':
		if(quoteMode){
		    this.temp = this.temp.append(c);
		    break;
		}
		if(this.temp.length() > 0){
		    genDTkFromTemp();
		    resetTemp();
		    break;
		}
		break;
	    default: 
		this.temp = this.temp.append(c);
		break;
	    }
	}
    }

    public LinkedList<Token> getTokens(){
	return this.tokens;
    }
    protected void resetTemp(){
	this.temp = new StringBuffer("");
    }
    protected void genDTkFromTemp(){
	this.tokens.add(new Token(this.temp.toString()));
    }
    protected void genQTkFromTemp(){
	this.tokens.add(new Token(this.temp.toString(),TokenType.QuoteTk));
    }
}
