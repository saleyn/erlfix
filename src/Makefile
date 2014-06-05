all: ../include/fix_parse.hrl ../ebin/fix_parse.beam ../ebin/fix.beam

fix_parse.erl: fix_parse_body.xsl FIX42.xml
#	java -cp /usr/share/java/saxon9he.jar net.sf.saxon.Transform -s:FIX42.xml -xsl:$< -o:$@ now=$(shell date +'%Y-%m-%d')
	xsltproc --stringparam author \
		"$(shell awk -F: '$$1 == u { print $$5 }' u=$(USER) /etc/passwd)" \
		--stringparam file "$(notdir $@)" \
		--stringparam now "$(shell date +'%Y-%m-%d')" $< $(word 2,$^) > $@

../include/fix_parse.hrl: fix_parse_header.xsl
	java -cp /usr/share/java/saxon9he.jar net.sf.saxon.Transform -s:FIX42.xml -xsl:$< -o:$@ now=$(shell date +'%Y-%m-%d')

../ebin/%.beam: %.erl
	erlc +debug_info +export_all -I../include -o ../ebin $<
