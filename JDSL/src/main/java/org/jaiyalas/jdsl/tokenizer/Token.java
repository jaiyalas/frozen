package org.jaiyalas.jdsl.tokenizer;

import java.util.StringTokenizer;
import java.lang.StringBuffer;

import java.util.LinkedList;
import java.util.Hashtable;

import java.util.function.*;


import org.jaiyalas.jdsl.tokenizer.TokenType;


public class Token{
    public static Token newlineTk = new Token(TokenType.NewlineTk);
    public static Token lbraceTk  = new Token(TokenType.LeftBraceTk);
    public static Token rbraceTk  = new Token(TokenType.RightBraceTk);
    public static Token seqTk     = new Token(TokenType.SeqTk);
    public static Token parTk     = new Token(TokenType.ParTk);
    public static Token colTk     = new Token(TokenType.ColonTk);

    public static Token getStaticTk(char _c){
	switch(_c){
	case '\n':
	    return Token.newlineTk;
	case '{':
	    return Token.lbraceTk;
	case '}':
	    return Token.rbraceTk;
	case ':':
	    return Token.colTk;
	case '|':
	    return Token.parTk;
	case ',':
	    return Token.seqTk;
	default:
	    return new Token();
	}
    }

    private String ctx;
    private TokenType type;

    public Token(){this("",TokenType.DefaultTk);}
    public Token(String _ctx, TokenType _tt){
	this.ctx = _ctx;
	this.type = _tt;
    }
    public Token(String _ctx){
	this(_ctx,TokenType.DefaultTk);
    }
    public Token(TokenType _tt){
	this("",_tt);
    }
    public String getCtx(){return this.ctx;} 
    public TokenType getType(){return this.type;}
};
