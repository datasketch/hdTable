test_that("multiplication works", {

  expect_equal( clean_names("sp ace"), "sp_ace" )
  expect_equal( clean_names(c("rep", "rep", "REP")), paste0("rep", c("","_1","_2")))
  expect_equal(clean_names("àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"),
               "aeiou_aeiouaeiouy_aeiouyaeiou_aeiouaeiou_aeioun_nc")
  expect_equal( clean_names("%"), "percent" )
  expect_equal( clean_names("€"), "euro" )
  expect_equal( clean_names("#"), "num" )
  expect_equal( clean_names("*"), "x" )

  expect_equal(clean_names(c("!@", "!", "^")), c("x", "x_1", "x_2"))
  expect_equal( clean_names("!"), "x" )
  expect_equal( clean_names(c("(", "!")), c("x", "x_1") )

  expect_equal( clean_names(c("*", "!")), c("x", "x_1") )

  expect_equal(clean_names("at___rcd_id"), "at___rcd_id")

  expect_equal(clean_names(""), "x")
  expect_equal( clean_names("can\"'t"), "cant" )
  expect_equal( clean_names("hi_`there`"), "hi_there" )

  expect_equal( clean_names("  leading spaces"), "leading_spaces" )
  expect_equal( clean_names("trailing spaces   "), "trailing_spaces" )
  expect_equal( clean_names("& spaces   "), "spaces" )
  expect_equal( clean_names("!leadingpunct"), "leadingpunct" )

  expect_equal( clean_names("a!!^@"), "a" )
  expect_equal( clean_names("d(!)9"), "d_9" )
  expect_equal( clean_names("a/b"), "a_b")

  expect_equal( clean_names("203-re--"), "x203_re")

  expect_equal( clean_names("ação"), "acao" )
  expect_equal( clean_names("Farœ"), "faroe" )
  expect_equal( clean_names("a b c d e f"), "a_b_c_d_e_f" )
  expect_equal( clean_names("testCamelCase"), "test_camel_case")
  expect_equal( clean_names("average # of days"), "average_num_of_days" )
  expect_equal( clean_names("jan2009sales"), "jan2009sales" )
  expect_equal( clean_names("jan 2009 sales"), "jan_2009_sales" )
  expect_equal( clean_names("unicode_µ"), "unicode_m" )
  expect_equal( clean_names("µ_first_unicode"), "m_first_unicode" )
  #expect_equal( clean_names("m\xb6"), "m")
  #expect_equal( clean_names("m\x83\x84\x85\x86\x87\xa1"), "mf" )
  #expect_equal( clean_names("m\xa9\xaa\xae\xb2\xb3\xb5\xbc\xbd\xbe\xc0"), "m_c_a_r_23m1_41_23_4a")
  #expect_equal( clean_names("m\u00b2"), "m2")


})
