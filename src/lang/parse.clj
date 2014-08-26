(ns lang.parse
  (:use [instaparse.core])
)


(def parse-query2
  (parser
   "
    sentence = (element|logic-bind|paren-bind) (paren-bind|logic-bind)*
    paren-bind = lparen logic-bind rparen
    logic-bind = (element|paren-bind) space logic space (element|logic-bind|paren-bind)
    element = (token|paren-bind)
    logic = (and|or)
    token = (phrase|word)
    space = #'\\s+'
    word = #'\\p{javaLowerCase}+'
    phrase = p word (space+ word)* p
    p = '\\\"'
    field = ':'
    and = <'AND'>
    or = <'OR'>
    not = <'NOT'>
    <lparen> = <'('>
    <rparen> = <')'>
   "
   )
)
  


(def parse-query3
  (parser
   "
    field-sentences = field-sentence+
    field-sentence = word field sentence space?
    sentence = (element|logic-bind|paren-bind) (paren-bind|logic-bind)*
    paren-bind = lparen logic-bind rparen
    logic-bind = (element|paren-bind) space logic space (element|logic-bind|paren-bind)
    <element> = (token|paren-bind)
    <logic> = (and|or|not)
    <token> = (phrase|word)
    <space> = <#'\\s+'>
    word = #'\\p{javaLowerCase}+'
    phrase = p word (space+ word)* p
    <p> = <'\\\"'>
    <field> = <#':'>
    and = <'AND'>
    or = <'OR'>
    not = <'NOT'>
    <lparen> = <'('>
    <rparen> = <')'>
   "
   )
)

(def parse-query
  (parser
   "
    field-sentences = (field-sentence|field-logic-bind|paren-bind) (paren-bind|field-logic-bind)*
    field-logic-bind = field-sentence space logic space (field-sentence|field-logic-bind)
    field-sentence = field-name field sentence space?
    <sentence> = (element|logic-bind|paren-bind) (paren-bind|logic-bind)*
    <paren-bind> = lparen (logic-bind|field-logic-bind) rparen
    logic-bind = (element|paren-bind) space logic space (element|logic-bind|paren-bind)
    <element> = (token|paren-bind)
    <logic> = (and|or|not)
    <token> = (phrase|word)
    <space> = <#'\\s+'>
    word = #'\\p{javaLowerCase}+[0-9]*'
    field-name =  #'\\p{javaLowerCase}+[0-9]*'
    phrase = p word (space+ word)* p
    <p> = <'\\\"'>
    <field> = <#':'>
    and = <'AND'>
    or = <'OR'>
    not = <'NOT'>
    <lparen> = <'('>
    <rparen> = <')'>
   "
   )
)
  


; try with (clojure.pprint/pprint (parse-query "field:\"xxx  vvv\" OR bbb AND (fto:yyy AND bbb OR xx:vvv)"))
