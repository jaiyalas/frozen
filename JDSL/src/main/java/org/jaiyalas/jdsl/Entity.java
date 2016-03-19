package org.jaiyalas.jdsl;

import java.lang.StringBuffer;
import java.util.LinkedList;

public class Entity{
    private LinkedList<String> labelEntity;
    private LinkedList<String> childrenEntity;
    private LinkedList<String> siblingEntity;
    private int counter;
    private boolean siblingFlag;
    public Entity(){
	this.labelEntity    = new LinkedList<String>();
	this.childrenEntity = new LinkedList<String>();
	this.siblingEntity  = new LinkedList<String>();
	this.counter = 0;
	this.siblingFlag = false;
    }
    public Entity(LinkedList<String> ts){
	this();
	ts.forEach((String t) -> {
		if(this.siblingFlag){
		    this.siblingEntity.add(t);
		}else{
		    if(this.counter == 0){
			if(t.equals("{")){
			    this.counter += 1;
			}else{
			    this.labelEntity.add(t);
			}
		    }else{
			if(t.equals("{")){
			    this.childrenEntity.add(t);
			    this.counter += 1;
			}else if(t.equals("}")){
			    if(counter == 1){
				this.siblingFlag = true;
			    }else{
				this.childrenEntity.add(t);
				this.counter -= 1;
			    }
			}else{
			    this.childrenEntity.add(t);
			}
		    }
		}
	    });
    }
    public LinkedList<String> getLabelEntity(){return this.labelEntity;}
    public LinkedList<String> getChildrenEntity(){return this.childrenEntity;}
    public LinkedList<String> getSiblingEntity(){return this.siblingEntity;}
}
