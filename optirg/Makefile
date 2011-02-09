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
	@echo "### Linking optirg/optirg ###"
	$(OCC) $(INC) optirg.cma main.cmo -o optirg
	chmod ug+x optirg 
	@echo ""

# Linking LIB
optirg.cma: $(IRG)/irg.cma formatlexer.cmo string_of_expr.cmo image_attr_size.cmo sw_fun.cmo irgp.cmo canon.cmo node_classes.cmo classfusion.cmo optirg.cmo 
	@echo "### Linking optirg/optirg.cma ###"
	$(OCC) $(INC) unix.cma str.cma $(IRG)/irg.cma formatlexer.cmo string_of_expr.cmo image_attr_size.cmo sw_fun.cmo irgp.cmo canon.cmo node_classes.cmo classfusion.cmo optirg.cmo -a -o optirg.cma
	@echo ""

# Compilation
main.cmo: $(IRG)/irg.cma main.ml optirg.cma
	@echo "### Compile optirg/main.ml ###"
	$(OCC) $(INC) unix.cma -c main.ml -o main.cmo 
	@echo ""

optirg.cmo: $(IRG)/irg.cma optirg.ml string_of_expr.cmo image_attr_size.cmo canon.cmo irgp.cmo classfusion.cmo
	@echo "### Compile optirg/optirg.ml ###"
	$(OCC) $(INC) -c optirg.ml -o optirg.cmo 
	@echo ""

classfusion.cmo: $(IRG)/irg.cma irgp.cmo sw_fun.cmo classfusion.ml node_classes.cmo
	@echo "### Compile optirg/classfusion.ml ###"
	$(OCC) $(INC) -c classfusion.ml -o classfusion.cmo 
	@echo ""

node_classes.cmo: $(IRG)/irg.cma irgp.cmo node_classes.ml
	@echo "### Compile optirg/classfusion.ml ###"
	$(OCC) $(INC) -c node_classes.ml -o node_classes.cmo 
	@echo ""

canon.cmo: $(IRG)/irg.cma canon.ml string_of_expr.cmo image_attr_size.cmo irgp.cmo
	@echo "### Compile optirg/canon.ml ###"
	$(OCC) $(INC) -c canon.ml -o canon.cmo 
	@echo ""

sw_fun.cmo: $(IRG)/irg.cma sw_fun.ml string_of_expr.cmo
	@echo "### Compile optirg/sw_fun.ml ###"
	$(OCC) $(INC) -c sw_fun.ml -o sw_fun.cmo 
	@echo ""

irgp.cmo: $(IRG)/irg.cma irgp.ml
	@echo "### Compile optirg/irgp.ml ###"
	$(OCC) $(INC) -c irgp.ml -o irgp.cmo 
	@echo ""

formatlexer.cmo: formatlexer.mll
	@echo "### Translate optirg/formatlexer.mll ###"
	ocamllex formatlexer.mll
	@echo "### Compile optirg/formatlexer.ml ###"
	$(OCC) $(INC) -c formatlexer.ml -o formatlexer.cmo
	@echo ""

string_of_expr.cmo: string_of_expr.ml $(IRG)/irg.cma
	@echo "### Compile optirg/string_of_expr.ml ###"
	$(OCC) $(INC) -c string_of_expr.ml -o string_of_expr.cmo 
	@echo ""

image_attr_size.cmo: image_attr_size.ml string_of_expr.cmo formatlexer.cmo
	@echo "### Compile optirg/image_attr_size.ml ###"
	$(OCC) $(INC) -c image_attr_size.ml -o image_attr_size.cmo 
	@echo ""

$(IRG)/irg.cma: 
	(cd $(IRG) ; make)

# Documentation 
doc: optirg.ml 
	@echo "### Make Documentation ###"
	mkdir -p $(DOCP)
	$(DOCC) $(INC) $(IRG)/irg.ml formatlexer.ml string_of_expr.ml image_attr_size.ml irgp.ml optirg.ml canon.ml node_classes.ml classfusion.ml -d $(DOCP)
	@echo ""

# Cleaning
clean: 
	@echo "### Clean optirg ###"
	rm -rf *.cmo *.cmi $(DOCP) formatlexer.ml
	@echo ""
	
distclean:
	@echo "### Clean optirg completely ###"
	rm -rf *.cmo *.cmi $(DOCP) formatlexer.ml optirg optirg.cma
	@echo ""

