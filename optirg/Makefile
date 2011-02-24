###################
# Makefile OptIRG #
###################



##############
# PARAMETERS #
##############

# Ocaml Compiler
OCC=ocamlc -g
# Ocaml Doc Compiler
DOCC=ocamldoc -html
# Doc path
DOCP=doc

# IRG path
IRG=../irg

# Includes
INC=-I $(IRG)



#########
# RULES #
#########

all: optirg.cma optirg 

# Linking EXEC
optirg: $(IRG)/irg.cma optirg.cma main.cmo
	$(OCC) $(INC) optirg.cma main.cmo -o optirg
	chmod ug+x optirg 

# Linking LIB
optirg.cma: $(IRG)/irg.cma formatlexer.cmo string_of_expr.cmo image_attr_size.cmo sw_fun.cmo irgp.cmo canon.cmo node_classes.cmo classfusion.cmo optirg.cmo 
	$(OCC) $(INC) unix.cma str.cma $(IRG)/irg.cma formatlexer.cmo string_of_expr.cmo image_attr_size.cmo sw_fun.cmo irgp.cmo canon.cmo node_classes.cmo classfusion.cmo optirg.cmo -a -o optirg.cma


# Compilation
main.cmo: $(IRG)/irg.cma main.ml optirg.cma
	$(OCC) $(INC) unix.cma -c main.ml -o main.cmo 


optirg.cmo: $(IRG)/irg.cma optirg.ml string_of_expr.cmo image_attr_size.cmo canon.cmo irgp.cmo classfusion.cmo
	$(OCC) $(INC) -c optirg.ml -o optirg.cmo 


classfusion.cmo: $(IRG)/irg.cma irgp.cmo sw_fun.cmo classfusion.ml node_classes.cmo
	$(OCC) $(INC) -c classfusion.ml -o classfusion.cmo 


node_classes.cmo: $(IRG)/irg.cma irgp.cmo node_classes.ml
	$(OCC) $(INC) -c node_classes.ml -o node_classes.cmo 


canon.cmo: $(IRG)/irg.cma canon.ml string_of_expr.cmo image_attr_size.cmo irgp.cmo
	$(OCC) $(INC) -c canon.ml -o canon.cmo 


sw_fun.cmo: $(IRG)/irg.cma sw_fun.ml string_of_expr.cmo
	$(OCC) $(INC) -c sw_fun.ml -o sw_fun.cmo 


irgp.cmo: $(IRG)/irg.cma irgp.ml
	$(OCC) $(INC) -c irgp.ml -o irgp.cmo 


formatlexer.cmo: formatlexer.mll
	ocamllex formatlexer.mll
	$(OCC) $(INC) -c formatlexer.ml -o formatlexer.cmo


string_of_expr.cmo: string_of_expr.ml $(IRG)/irg.cma
	$(OCC) $(INC) -c string_of_expr.ml -o string_of_expr.cmo 


image_attr_size.cmo: image_attr_size.ml string_of_expr.cmo formatlexer.cmo
	$(OCC) $(INC) -c image_attr_size.ml -o image_attr_size.cmo 


$(IRG)/irg.cma: 
	(cd $(IRG) ; make)

# Documentation 
doc: optirg.ml 
	mkdir -p $(DOCP)
	$(DOCC) $(INC) $(IRG)/irg.ml formatlexer.ml string_of_expr.ml image_attr_size.ml irgp.ml optirg.ml canon.ml node_classes.ml classfusion.ml -d $(DOCP)


# Cleaning
clean: 
	rm -rf *.cmo *.cmi $(DOCP) formatlexer.ml

	
distclean:
	rm -rf *.cmo *.cmi $(DOCP) formatlexer.ml optirg optirg.cma


