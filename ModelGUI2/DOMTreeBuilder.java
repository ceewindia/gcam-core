/*!
*	\file DOMTreeBuilder.java
*	\ingroup CAIM
*	\brief The DOMTreeBuilder class to build a DOM tree from CSV data.
*	\author Pralit Patel
*	\author Kathrine Chung
*	\date $Date$
*	\version $Revision$
*/

import java.io.FileWriter;
import java.util.*;
import org.w3c.dom.*;
import org.w3c.dom.bootstrap.*;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;

/*!
*	\ingroup CAIM
*       \brief Converts CSV formated tables to XML.
*
*	Class creates a DOM document from CSV, adds to the tree using specified header info,
*	and data array. Can also output the resulting XML to a specified file.
*
*	\author Pralit Patel
*	\author Katherine Chung
*	\warning User need to have the DOM level 3 implementation installed, currently BETA for java 1.5.0 and Xerces 2.6.2
*/

public class DOMTreeBuilder {

	private Document doc; //!< Keeps the instance of the DOM document being used to store the XML tree.
	private Headers head; //!< Has the parsed header info necessary to build nodes.
	private ArrayList dataArr; //!< Current line of data being processed.
	private Node rootElement; //!< Holds the pointer to the root of the DOM tree
	private String docName; //!< Holds the tagName of the root

	/*! \brief Default constructor.
	*
	*   Initializes the document by loading up the DOMImplementation for XML 3.0
	*
	*  \warning User need to have the DOM level 3 implementation installed, currently BETA for java 1.5.0 and Xerces 2.6.2
	*/
	public DOMTreeBuilder() {
		docName = "scenario";
		try {
			System.setProperty(DOMImplementationRegistry.PROPERTY, "org.apache.xerces.dom.DOMImplementationSourceImpl");
			DOMImplementationRegistry reg = DOMImplementationRegistry.newInstance();
			DOMImplementation impl = reg.getDOMImplementation( "XML 3.0" );
		    	if (impl == null) {
		    		System.out.println("Could not find a DOM3 Load-Save compliant parser.");
			        return;
			}
			// Create the document
			DocumentType svgDOCTYPE = impl.createDocumentType(docName, "","");
			doc = impl.createDocument("http://www.w3.org/2000/svg", docName, svgDOCTYPE);
			rootElement = doc.getDocumentElement();
		} catch (Exception e) {
			System.err.println(e);
		}
	}

	/* \brief Set the header.
	*
	*  Setting the header specified the format of the data.
	*
	*/
	public void setHeader(String headerIn) throws Exception {
		head = new Headers(headerIn.split(","));
	}

	/*! \brief Adds the current data to the Tree
	*
	*   calls makeTree using the data passed in and the current Headers set to
	*   specify how to create the Nodes.
	*
	*   \warning user must set a header before calling this function.
	*/
	public void addToTree (ArrayList data) {
		if (head == null) {
			System.out.println("Warning, no header set, skipping data");
			return;
		}
		dataArr = data;
		makeTree(rootElement, docName);
	}

	/* \brief Outputs the Tree in XML format to filename.
	*
	*  Uses standard XML serializers to process and output the DOM tree.
	*
	*/
	public void outputTree(String filename) {
		// specify output formating properties
		OutputFormat format = new OutputFormat(doc);
		format.setEncoding("UTF-8");
		format.setLineSeparator("\n");
		format.setIndenting(true);
		format.setIndent(3);
		format.setLineWidth(0);
		format.setPreserveSpace(false);
		format.setOmitDocumentType(true);

		// create the searlizer and have it print the document
		try {
			XMLSerializer serializer = new XMLSerializer (new FileWriter(filename), format);
			serializer.asDOMSerializer();
			serializer.serialize(doc);
		} catch (java.io.IOException e) {
			System.err.println("Error outputing tree: "+e);
		}
	}

	public Document getDoc() {
		return doc;
	}

	/*! \brief Compare to Individual Elements of a DOM tree.
	*
	*   Elements are considered to be the same if they both have the same tag name,
	*   same attrubutes, and same TEXT data if any.
	*
	*   \return Returns true if the elements are the same, false otherwise.
	*/
	public static boolean compareHelper(Element e1, Element e2){ // helper for compare function
		// first make sure tag names are the same
		if( !((e1.getTagName()).equals(e2.getTagName()) ) ){
			return false;
		}

		// go through all the attributes, make sure have the same ammount and the have the same values
		NamedNodeMap attrs1 = e1.getAttributes();
		NamedNodeMap attrs2 = e2.getAttributes();
		String temp;
		if (attrs1.getLength() != attrs2.getLength()) {
			return false;
		}
		for (int i = 0; i < attrs1.getLength(); i++) {
			temp = attrs1.item(i).getNodeName();
			if (!(e1.getAttribute(temp).equals(e2.getAttribute(temp)))) {
				return false;
			}
		}
		// go through the children and look for a TEXT_NODE, and check to make sure the data in them are
		// the same
		NodeList child1 = e1.getChildNodes();
		NodeList child2 = e2.getChildNodes();
		int textchild1 = -1;
		int textchild2 = -1;
		for(int i=0; i<child1.getLength(); i++){
			if(child1.item(i).getNodeType() == Element.TEXT_NODE)
				textchild1 = i;
		}
		for(int i=0; i<child2.getLength(); i++){
			if(child2.item(i).getNodeType() == Element.TEXT_NODE)
				textchild2 = i;
		}
		if((textchild1 == -1) && (textchild2 == -1)){
			return true;
		}
		return true;
		/*
		if(textchild1 != -1 && textchild2 != -1 ){
			if(!child1.item(textchild1).getNodeValue().equals (child2.item(textchild2).getNodeValue())) {
				return false;
			}
			else{
				return true;
			}
		}else{
			return false;
		}
		*/
	}

	/*! \brief Compare all of the imediate children of e1 to e2.
	*
	*   Compares each element and if any of the children are the same as e2
	*   it shall be returned.
	*
	*   \return Returns the Node which is the same as e2 and already exists in the tree, or null.
	*/
	public Node compare( Element e1, Element e2 ){
		// get all the immediate children of e1 and compare them to e2
		NodeList list1 = e1.getChildNodes();
		for(int i=0; i<list1.getLength(); i++){
			if(list1.item(i).getNodeType() != Element.TEXT_NODE){
				if(compareHelper((Element)list1.item(i), e2)){
					return list1.item(i);
				}
			}
		}
		return null;
	}

	private Node hasTextData(Node curr) {
		NodeList list1 = curr.getChildNodes();
		for(int i=0; i<list1.getLength(); i++){
			if(list1.item(i).getNodeType() == Element.TEXT_NODE){
				return list1.item(i);
			}
		}
		return null;
	}

	/*! \brief Creates the DOM tree.
	*
	*   Starts from the root of the tree, and creates nodes in the appropriate positions,
	*   determined through the headers, fills nodes with the correct current data.
	*
	*/
	private void makeTree( Node parent, String name) {
                String chName;
		int retInt;
                ArrayList children = head.getIndecesWhereParentHeaderIs(name);
                Element tempNode;
                Text tempText = null;
		Node retNode;
		String attrStr;
		Object retVal;
		Iterator it = children.iterator();
		int currPos;
		// loop through all the headers that are the immediate child of name
                //for (int i = 0; i < children.size(); i++) {
		while(it.hasNext()) {
			currPos = ((Integer)it.next()).intValue();
                        chName = head.getChildHeader(currPos);
			// this indicates that the child with specify which parent it belongs under
			if (head.checkChildStackFront() == currPos ) {
				head.popChildStack();
			}
			if ( chName.startsWith( "*" ) ) {
				makeTreeHelper( parent, chName.substring( 1, chName.length() ) );
			}
			else {
                        	tempNode = doc.createElement(chName);
				// look up and get proper attribute names and it's value
                        	if ( !(attrStr = head.isAttribute(chName)).equals("")  ) {
					retVal = head.getAttribute(chName+"\\"+attrStr);
					if ( retVal == null ) {
						retVal = head.getAttribute(name+"\\"+chName+"\\"+attrStr);
					}
					if ( retVal instanceof Integer ) {
                                		tempNode.setAttribute(attrStr,(String)dataArr.get(((Integer)retVal).intValue()));
					}
					else if ( retVal instanceof String) {
						tempNode.setAttribute(attrStr,(String)retVal);
					}
					else if ( retVal instanceof ArrayList) {
						//System.out.println("In array part for "+chName+" with no ch "+((ArrayList)retVal).size());
						Iterator itTemp = it;
						while(itTemp.hasNext()) {
							if (head.getChildHeader(((Integer)itTemp.next()).intValue()).equals(chName)) {
								itTemp.remove();
							}
						}
						it = children.iterator();
						while(it.hasNext() && ((Integer)it.next()).intValue() != currPos) {}
						for (int j = 0; j < ((ArrayList)retVal).size(); j++) {
							if (j != 0) {
								tempNode = doc.createElement(chName);
							}
							tempNode.setAttribute(attrStr,(String)((ArrayList)retVal).get(j));
							if ((retInt = head.isData(head.getParentHeader(currPos)+"\\"+chName+"\\"+attrStr+"\\"+(String)((ArrayList)retVal).get(j))) != -1 ) {
								if ( (retInt+1) > dataArr.size()) {
									System.out.println("FAILED: dataArr.get(retInt), array bounds exceeded "+(retInt+1)+" > "+dataArr.size());
								}
								tempText = doc.createTextNode((String)dataArr.get(retInt));
								tempNode.appendChild(tempText);
							}
							// check to see if this node exists already so as to not duplicate it, and
							// recurse using this child as the new parent
							if ((retNode = compare((Element)parent, tempNode)) == null) {
								parent.appendChild(tempNode);
								makeTree(tempNode, chName);
							} else {
								if ((hasTextData(retNode)) != null) {
									(hasTextData(retNode)).setNodeValue(tempText.getNodeValue());
								}
								makeTree(retNode, chName);
                        				}
						}
						continue;
					}
                        	}
				// checks to see if this node should have data and set it properly if so
                        	if ( (retInt = head.isData(head.getParentHeader(currPos)+"\\"+chName)) != -1 ) {
					if ( (retInt+1) > dataArr.size()) {
						System.out.println("FAILED: dataArr.get(retInt), array bounds exceeded "+(retInt+1)+" > "+dataArr.size());
					}
                               		tempText = doc.createTextNode((String)dataArr.get(retInt));
                                	tempNode.appendChild(tempText);
                        	}
				// check to see if this node exists already so as to not duplicate it, and
				// recurse using this child as the new parent
                        	if ((retNode = compare((Element)parent, tempNode)) == null) {
                                	parent.appendChild(tempNode);
                                	makeTree(tempNode, chName);
                        	} else {
					if ((hasTextData(retNode)) != null) {
						(hasTextData(retNode)).setNodeValue(tempText.getNodeValue());
					}
                                	makeTree(retNode, chName);
                        	}
			}
                }
        }

	/*! \brief Helper for the makeTree function
	*
	*   Takes care of the unusual case when the child specifies exactly which parent
	*   it should nest under, ex: {name=Ref Oil}Input/AEEI.
	*
	*/
	private void makeTreeHelper( Node parent, String name ) {
		ArrayList children = head.getIndecesWhereParentHeaderIs(name);
		String parentName;
		String attrStr;
		Element tempNode;
		Node retNode;
		int retInt = 0;
		boolean isExt = false;
		// go through all the children, and create their parent as they specify
		for (int i = 0; i < children.size(); i++ ) {
			// need to parse this parent header as they will not come in parsed
			parentName = head.getParentHeader(((Integer)children.get(i)).intValue());
			if (parentName.split("/").length > 1) {
			//if (parentName.matches("/../")) {
				head.addToChildStack(((Integer)children.get(i)).intValue());
				parentName = parentName.substring(0,parentName.indexOf("/"));
				isExt = true;
			}
			if (parentName.startsWith("{")) {
				retInt = parentName.indexOf("}");
				attrStr = parentName.substring(1,retInt);
				tempNode = doc.createElement(parentName.substring(retInt+1, parentName.length()));
				if (isExt) {
					parentName = '@' + parentName.substring(retInt+1);
				}
				retInt = attrStr.indexOf("=");
				tempNode.setAttribute(attrStr.substring(0,retInt), attrStr.substring(retInt+1, attrStr.length()));
			}
			else {
				tempNode = doc.createElement(parentName);
			}
			// same idea as makeTree, where we need to avoid duplicates, the child should now be
			// able to recurse normally
			if ((retNode = compare((Element)parent, tempNode)) == null) {
                               	parent.appendChild(tempNode);
                                makeTree(tempNode, parentName);
                        } else {
                                makeTree(retNode, parentName);
                        }
		}
	}
}