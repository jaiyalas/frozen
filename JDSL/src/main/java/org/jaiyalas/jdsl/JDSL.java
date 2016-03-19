package org.jaiyalas.jdsl;

import java.util.StringTokenizer;
import java.lang.StringBuffer;

import java.util.LinkedList;
import java.util.NoSuchElementException;

import java.util.Hashtable;

import java.util.function.*;

import org.jaiyalas.jdsl.Entity;
import org.jaiyalas.jdsl.accumulator.*;
import org.jaiyalas.jdsl.tokenizer.*;

public class JDSL{
    public static void main(String[] args){
	// tesing context
	String ctx = 
	    "A0 a0 { \nB: b0 | b 1 \n Yo : Y0 | Y1\n}\n A1 a1 { \nC \"this is shit\" \n D d \n}";

	Tokenizer tker = new Tokenizer(ctx);

	// tokens 
	LinkedList<Token> tokens = tker.getTokens();

	//print tokens
	tokens.forEach(t -> {
		System.out.println(t.getType().toString()+"("+t.getCtx()+")");
	    });
	System.out.println("#######");
	
	/**
	Entity ent = new Entity(tokens);
		
	ent.getLabelEntity().forEach(System.out::println);
	System.out.println("=======");
	ent.getChildrenEntity().forEach(System.out::println);
	System.out.println("=======");
	ent.getSiblingEntity().forEach(System.out::println);

	// consumer!!
	RuleBase eat = new RuleBase();
	Accumulator recorder = new Accumulator();
	try{	    
	    eat.consumeOneToken(recorder,tokens);
	}
	catch(NoSuchElementException e){
	    //
	}
	*/
    };
};

class RuleBase{
    // The type variable <T> should be 
    //the structure of the parsing results or its sub-structure   
    private Hashtable<String, BiFunction<Accumulator, LinkedList<String>, Accumulator>> rtable;

    public RuleBase(){
	rtable = new Hashtable<String, BiFunction<Accumulator, LinkedList<String>, Accumulator>>();
	rtable.put("search",   Rule.r_search);
	rtable.put("document", Rule.r_document);
	rtable.put("field",    Rule.r_field);
    };

    public boolean addRule(String _key, BiFunction<Accumulator, LinkedList<String>, Accumulator> _rule){
	if(this.rtable.containsKey(_key)){
	    rtable.replace(_key,_rule);
	    return true;
	}
	rtable.put(_key, _rule);
	return false;
    }
    /**
    public LinkedList<String> consumeOneToken(Accumulator acc, LinkedList<String> tokens)
	throws NoSuchElementException{
	LinkedList<String> res;
	String tLabel = tokens.pop();
	BiFunction<Accumulator, LinkedList<String>, Accumulator> rule = 
	    this.rtable.getOrDefault(tLabel,(Accumulator _acc, LinkedList<String> _ts) -> {return _acc;});
	res = rule.apply(acc, tokens);
	return res;
    };
    */
};


class Rule{
    public static BiFunction<Accumulator, LinkedList<String>, Accumulator> r_search = (Accumulator acc, LinkedList<String> ts) -> {
	return acc;
    };
    public static BiFunction<Accumulator, LinkedList<String>, Accumulator> r_document = (Accumulator acc, LinkedList<String> ts) -> {
	return acc;
    };
    public static BiFunction<Accumulator, LinkedList<String>, Accumulator> r_field = (Accumulator acc, LinkedList<String> ts) -> {
	return acc;
    };
}
