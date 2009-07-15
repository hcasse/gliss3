###################
# Makefile OptIRG #
###################



##############
# PARAMETERS #
##############

# Ocaml Compiler
OCC=ocamlc
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
optirg.cma: $(IRG)/irg.cma formatlexer.cmo string_of_expr.cmo image_attr_size.cmo optirg.cmo
	@echo "### Linking optirg/optirg.cma ###"
	$(OCC) $(INC) str.cma $(IRG)/irg.cma formatlexer.cmo string_of_expr.cmo image_attr_size.cmo optirg.cmo -a -o optirg.cma
	@echo ""

# Compilation
main.cmo: $(IRG)/irg.cma main.ml optirg.cma
	@echo "### Compile optirg/main.ml ###"
	$(OCC) $(INC) -c main.ml -o main.cmo 
	@echo ""

optirg.cmo: $(IRG)/irg.cma optirg.ml string_of_expr.cmo image_attr_size.cmo
	@echo "### Compile optirg/optirg.ml ###"
	$(OCC) $(INC) -c optirg.ml -o optirg.cmo 
	@echo ""

formatlexer.cmo: formatlexer.mll
	@echo "### Translate optirg/formatlexer.mll ###"
	ocamllex formatlexer.mll
	@echo "### Compile optirg/formatlexer.ml ###"
	$(OCC) formatlexer.ml
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
	$(DOCC) $(INC) $(IRG)/irg.cma optirg.cmo -d $(DOCP)
	@echo ""

# Cleaning
clean: 
	(cd $(IRG) ; make clean)
	@echo "### Clean optirg ###"
	rm -rf *.cmo *.cmi $(DOCP)
	@echo ""

