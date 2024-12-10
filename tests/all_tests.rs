mod infra;

// Your tests go here!
success_tests! {
    // Boa Regression Tests
    {
        name: boa_binding1,
        file: "boa/binding1.bet",
        expected: "-5",
    },

        ////// BEGIN COBRA PROVIDED TESTS
    // Tests we gave students
    {
        name: cobra_false_val,
        file: "cobra/false_val.bet",
        expected: "false",
    },
    {
        name: cobra_example1,
        file: "cobra/example1.bet",
        expected: "6",
    },
    {
        name: cobra_example2,
        file: "cobra/example2.bet",
        expected: "-6",
    },
    {
        name: cobra_factorial0,
        file: "cobra/factorial.bet",
        input: "0",
        expected: "1",
    },
    {
        name: cobra_factorial1,
        file: "cobra/factorial.bet",
        input: "1",
        expected: "1",
    },
    {
        name: cobra_factorial2,
        file: "cobra/factorial.bet",
        input: "2",
        expected: "2",
    },
    {
        name: cobra_factorial3,
        file: "cobra/factorial.bet",
        input: "3",
        expected: "6",
    },
    {
        name: cobra_input_compare_1,
        file: "cobra/input_compare.bet",
        input: "2",
        expected: "false",
    },
    {
        name: cobra_input_compare_2,
        file: "cobra/input_compare.bet",
        input: "10",
        expected: "true",
    },
    // Number and Boolean Literals
    {
        name: cobra_num,
        file: "cobra/num.bet",
        expected: "644",
    },

    // Input Expression
    {
        name: cobra_input_default,
        file: "cobra/input0.bet",
        expected: "0",
    },
    {
        name: cobra_input_num,
        file: "cobra/input0.bet",
        input: "123",
        expected: "123",
    },

    // Simple Number Expressions
    {
        name: boa_add1,
        file: "boa/add1.bet",
        expected: "73",
    },
    {
        name: boa_add1_sub1,
        file: "boa/add1_sub1.bet",
        expected: "4",
    },
    {
        name: cobra_add_num,
        file: "cobra/add.bet",
        input: "10",
        expected: "15",
    },

    // Nested Arithmetic Expressions
    {
        name: boa_nested_arith0,
        file: "boa/nested_arith0.bet",
        expected: "35",
    },
    {
        name: boa_nested_arith1,
        file: "boa/nested_arith1.bet",
        expected: "25",
    },
    {
        name: boa_nested_arith2,
        file: "boa/nested_arith2.bet",
        expected: "0",
    },
    {
        name: boa_nested_arith3,
        file: "boa/nested_arith3.bet",
        input: "8",
        expected: "1117",
    },
    {
        name: boa_nested_arith4,
        file: "boa/nested_arith4.bet",
        expected: "-1",
    },

    // Comparison Expressions
    {
        name: cobra_equals_bool,
        file: "cobra/equals_bool.bet",
        expected: "true",
    },
    {
        name: cobra_compare_expr_succ0,
        file: "cobra/compare_expr_succ0.bet",
        expected: "true",
    },

    {
        name: cobra_compare_expr_succ2,
        file: "cobra/compare_expr_succ2.bet",
        expected: "true",
    },

    // Let expressions
    {
        name: cobra_binding0,
        file: "cobra/binding0.bet",
        expected: "5",
    },
    {
        name: cobra_binding1,
        file: "cobra/binding1.bet",
        expected: "-5",
    },

    {
        name: cobra_binding_expr,
        file: "cobra/binding_expr.bet",
        expected: "1225",
    },
    {
        name: boa_binding_nested,
        file: "boa/binding_nested.bet",
        expected: "1",
    },

    {
        name: cobra_binding_chain,
        file: "cobra/binding_chain.bet",
        expected: "3",
    },
    {
        name: cobra_binding_nested_chain,
        file: "cobra/binding_nested_chain.bet",
        expected: "12",
    },

    // Let expressions with shadowing
    {
        name: boa_shadowed_binding_succ0,
        file: "boa/shadowed_binding_succ0.bet",
        expected: "100",
    },
    {
        name: boa_shadowed_binding_succ1,
        file: "boa/shadowed_binding_succ1.bet",
        expected: "7",
    },
    {
        name: boa_shadowed_binding_succ2,
        file: "boa/shadowed_binding_succ2.bet",
        expected: "150",
    },
    {
        name: boa_shadowed_binding_succ3,
        file: "boa/shadowed_binding_succ3.bet",
        expected: "5",
    },
    {
        name: boa_shadowed_binding_succ4,
        file: "boa/shadowed_binding_succ4.bet",
        expected: "18",
    },
    {
        name: boa_shadowed_binding_succ5,
        file: "boa/shadowed_binding_succ5.bet",
        expected: "5",
    },
    {
        name: boa_shadowed_binding_succ6,
        file: "boa/shadowed_binding_succ6.bet",
        expected: "3",
    },
    {
        name: boa_shadowed_binding_succ7,
        file: "boa/shadowed_binding_succ7.bet",
        expected: "200",
    },

    // Misc complex expressions with arithmetic and let bindings
    {
        name: boa_complex_expr,
        file: "boa/complex_expr.bet",
        expected: "6",
    },
    {
        name: boa_quick_brown_fox,
        file: "boa/quick_brown_fox.bet",
        expected: "-3776",
    },

    // If expressions
    {
        name: cobra_if_expr_succ2,
        file: "cobra/if_expr_succ2.bet",
        expected: "8",
    },
    {
        name: cobra_if_expr_succ3,
        file: "cobra/if_expr_succ3.bet",
        expected: "7",
    },

    // Set expr
    {
        name: cobra_set_expr_succ0,
        file: "cobra/set_expr1.bet",
        expected: "10",
    },
    {
        name: cobra_set_expr_succ1,
        file: "cobra/set_expr2.bet",
        expected: "true",
    },
    {
        name: cobra_set_expr_succ2,
        file: "cobra/set_expr3.bet",
        input: "25",
        expected: "true",
    },
    {
        name: cobra_set_expr_succ3,
        file: "cobra/set_expr3.bet",
        input: "20",
        expected: "false",
    },

    {
        name: cobra_loop_expr_succ0,
        file: "cobra/loop_expr0.bet",
        input: "3",
        expected: "6",
    },
    {
        name: cobra_loop_expr_succ1,
        file: "cobra/loop_expr0.bet",
        input: "7",
        expected: "5040",
    },
    {
        name: cobra_loop_expr_succ2,
        file: "cobra/loop_expr1.bet",
        expected: "-6",
    },

    ////// END COBRA PROVIDED TESTS

    // Unit Tests
    {
        name: cobra_input_plus1_1,
        file: "cobra/input_plus1.bet",
        input: "10",
        expected: "11",
    },
    {
        name: cobra_input_plus1_2,
        file: "cobra/input_plus1.bet",
        input: "0",
        expected: "1",
    },
    {
        name: cobra_true_val,
        file: "cobra/true_val.bet",
        expected: "true",
    },
    {
        name: cobra_negative_one_val,
        file: "cobra/negative_one_val.bet",
        expected: "-1",
    },
    {
        name: cobra_zero_val,
        file: "cobra/zero_val.bet",
        expected: "0",
    },
    {
        name: cobra_one_val,
        file: "cobra/one_val.bet",
        expected: "1",
    },
    {
        name: cobra_five_val,
        file: "cobra/five_val.bet",
        expected: "5",
    },
    {
        name: cobra_compare_greater_1,
        file: "cobra/compare_greater.bet",
        input: "6",
        expected: "true",
    },
    {
        name: cobra_compare_greater_2,
        file: "cobra/compare_greater.bet",
        input: "5",
        expected: "false",
    },
    {
        name: cobra_compare_greater_3,
        file: "cobra/compare_greater.bet",
        input: "4",
        expected: "false",
    },
    {
        name: cobra_compare_greater_equal_1,
        file: "cobra/compare_greater_equal.bet",
        input: "6",
        expected: "true",
    },
    {
        name: cobra_compare_greater_equal_2,
        file: "cobra/compare_greater_equal.bet",
        input: "5",
        expected: "true",
    },
    {
        name: cobra_compare_greater_equal_3,
        file: "cobra/compare_greater_equal.bet",
        input: "4",
        expected: "false",
    },
    {
        name: cobra_compare_equal_1,
        file: "cobra/compare_equal.bet",
        input: "6",
        expected: "false",
    },
    {
        name: cobra_compare_equal_2,
        file: "cobra/compare_equal.bet",
        input: "5",
        expected: "true",
    },
    {
        name: cobra_compare_equal_3,
        file: "cobra/compare_equal.bet",
        input: "4",
        expected: "false",
    },
    {
        name: cobra_compare_less_equal_1,
        file: "cobra/compare_less_equal.bet",
        input: "6",
        expected: "false",
    },
    {
        name: cobra_compare_less_equal_2,
        file: "cobra/compare_less_equal.bet",
        input: "5",
        expected: "true",
    },
    {
        name: cobra_compare_less_equal_3,
        file: "cobra/compare_less_equal.bet",
        input: "4",
        expected: "true",
    },
    {
        name: cobra_compare_less_1,
        file: "cobra/compare_less.bet",
        input: "6",
        expected: "false",
    },
    {
        name: cobra_compare_less_2,
        file: "cobra/compare_less.bet",
        input: "5",
        expected: "false",
    },
    {
        name: cobra_compare_less_3,
        file: "cobra/compare_less.bet",
        input: "4",
        expected: "true",
    },
    {
        name: cobra_equal_bool,
        file: "cobra/equal_bool.bet",
        expected: "true",
    },
   {
        name: cobra_not_equal_bool,
        file: "cobra/not_equal_bool.bet",
        expected: "false",
    },
    {
        name: cobra_good_set,
        file: "cobra/good_set.bet",
        expected: "6",
    },
    {
        name: cobra_basic_block,
        file: "cobra/basic_block.bet",
        expected: "3",
    },
    {
        name: cobra_nested_loops_1,
        file: "cobra/nested_loops.bet",
        input: "1",
        expected: "1",
    },
    {
        name: cobra_nested_loops_2,
        file: "cobra/nested_loops.bet",
        input: "5",
        expected: "125",
    },
    {
        name: cobra_nested_loops_3,
        file: "cobra/nested_loops.bet",
        input: "0",
        expected: "0",
    },
    {
        name: cobra_nested_loops_4,
        file: "cobra/nested_loops.bet",
        input: "1000",
        expected: "1000000000",
    },

    // Ensemble Tests
    {
        name: cobra_big_negative_input,
        file: "cobra/just_input.bet",
        input: "-9223372036854775808",
        expected: "-9223372036854775808",
    },
    {
        name: cobra_equal_bool_compound,
        file: "cobra/equal_bool_compound.bet",
        expected: "true",
    },
    {
        name: cobra_equal_bool_let,
        file: "cobra/equal_bool_let.bet",
        expected: "true",
    },
    {
        name: cobra_if_nested,
        file: "cobra/if_nested.bet",
        expected: "5",
    },
    {
        name: cobra_if_let,
        file: "cobra/if_let.bet",
        expected: "5",
    },
    {
        name: cobra_ok_positive_input,
        file: "cobra/just_input.bet",
        input: "9223372036854775807",
        expected: "9223372036854775807",
    },
    {
        name: diamondback_print_int,
        file: "diamondback/print_int.bet",
        expected: "5\n5",
    },
    {
        name: diamondback_print_bool,
        file: "diamondback/print_bool.bet",
        expected: "true\ntrue",
    },
    {
        name: diamondback_print_test,
        file: "diamondback/print_test.bet",
        input: "3",
        expected: "5\n4\n3\n3",
    },
    {
        name: diamondback_let_print,
        file: "diamondback/let_print_test.bet",
        expected: "5\n5",
    },
    {
        name: diamondback_sophia_test,
        file: "diamondback/sophia_test.bet",
        expected: "243\n244",
    },
    {
        name: diamondback_fact_5,
        file: "diamondback/fact.bet",
        input: "5",
        expected: "120"
    },
    {
        name: diamondback_fact_10,
        file: "diamondback/fact.bet",
        input: "10",
        expected: "3628800"
    },
    {
        name: diamondback_even_odd_one_print,
        file: "diamondback/even_odd_one_print.bet",
        input: "1",
        expected: "false"
    },
    {
        name: diamondback_even_odd_11_print,
        file: "diamondback/even_odd_one_print.bet",
        input: "11",
        expected: "false"
    },
    {
        name: diamondback_even_odd_1000_print,
        file: "diamondback/even_odd.bet",
        input: "1000",
        expected: "1000\ntrue\ntrue"
    },
    {
        name: diamondback_fib_1,
        file: "diamondback/fib.bet",
        input: "2",
        expected: "1\n1\n2"
    },
    {
        name: diamondback_fib_2,
        file: "diamondback/fib.bet",
        input: "10",
        expected: "34\n55\n89"
    },
    {
        name: diamondback_fib_3,
        file: "diamondback/fib.bet",
        input: "18",
        expected: "1597\n2584\n4181"
    },
    {
        name: diamondback_many_args,
        file: "diamondback/many_args.bet",
        input: "5",
        expected: "760"
    },
    {
        name: diamondback_add_up_1,
        file: "diamondback/add_up.bet",
        input: "1",
        expected: "1"
    },
    {
        name: diamondback_add_up_2,
        file: "diamondback/add_up.bet",
        input: "5",
        expected: "15"
    },
    {
        name: diamondback_add_up_3,
        file: "diamondback/add_up.bet",
        input: "17",
        expected: "153"
    },
    {
        name: diamondback_print_up_to_1,
        file: "diamondback/print_up_to.bet",
        input: "2",
        expected: "1\n1\n2\n4"
    },
    {
        name: diamondback_print_up_to_2,
        file: "diamondback/print_up_to.bet",
        input: "4",
        expected: "1\n1\n2\n4\n3\n9\n4\n16"
    },

    ///// New-to-Diamondback Auto Grader
    {
        name: diamondback_even_odd_1,
        file: "diamondback/even_odd.bet",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: diamondback_even_odd_2,
        file: "diamondback/even_odd.bet",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: diamondback_fun_nested_call,
        file: "diamondback/fun_nested_call.bet",
        input: "20",
        expected: "2100",
    },
    {
        name: diamondback_fun_two_args,
        file: "diamondback/fun_two_args.bet",
        expected: "25",
    },
    {
        name: diamondback_fun_many_args,
        file: "diamondback/fun_many_args.bet",
        input: "-1",
        expected: "2147483648",
    },
    {
        name: diamondback_fun_many_calls,
        file: "diamondback/fun_many_calls.bet",
        input: "5",
        expected: "0\n5\n10\n15\n20\n25\n0",
    },
    // Printing / Function Calls
    {
        name: diamondback_fun_many_prints,
        file: "diamondback/fun_many_calls.bet",
        input: "999",
        expected: "0\n999\n1998\n2997\n3996\n4995\n5994\n6993\n7992\n8991\n9990\n10989\n11988\n12987\n13986\n14985\n15984\n16983\n17982\n18981\n19980\n20979\n21978\n22977\n23976\n24975\n25974\n26973\n27972\n28971\n29970\n30969\n31968\n32967\n33966\n34965\n35964\n36963\n37962\n38961\n39960\n40959\n41958\n42957\n43956\n44955\n45954\n46953\n47952\n48951\n49950\n50949\n51948\n52947\n53946\n54945\n55944\n56943\n57942\n58941\n59940\n60939\n61938\n62937\n63936\n64935\n65934\n66933\n67932\n68931\n69930\n70929\n71928\n72927\n73926\n74925\n75924\n76923\n77922\n78921\n79920\n80919\n81918\n82917\n83916\n84915\n85914\n86913\n87912\n88911\n89910\n90909\n91908\n92907\n93906\n94905\n95904\n96903\n97902\n98901\n99900\n100899\n101898\n102897\n103896\n104895\n105894\n106893\n107892\n108891\n109890\n110889\n111888\n112887\n113886\n114885\n115884\n116883\n117882\n118881\n119880\n120879\n121878\n122877\n123876\n124875\n125874\n126873\n127872\n128871\n129870\n130869\n131868\n132867\n133866\n134865\n135864\n136863\n137862\n138861\n139860\n140859\n141858\n142857\n143856\n144855\n145854\n146853\n147852\n148851\n149850\n150849\n151848\n152847\n153846\n154845\n155844\n156843\n157842\n158841\n159840\n160839\n161838\n162837\n163836\n164835\n165834\n166833\n167832\n168831\n169830\n170829\n171828\n172827\n173826\n174825\n175824\n176823\n177822\n178821\n179820\n180819\n181818\n182817\n183816\n184815\n185814\n186813\n187812\n188811\n189810\n190809\n191808\n192807\n193806\n194805\n195804\n196803\n197802\n198801\n199800\n200799\n201798\n202797\n203796\n204795\n205794\n206793\n207792\n208791\n209790\n210789\n211788\n212787\n213786\n214785\n215784\n216783\n217782\n218781\n219780\n220779\n221778\n222777\n223776\n224775\n225774\n226773\n227772\n228771\n229770\n230769\n231768\n232767\n233766\n234765\n235764\n236763\n237762\n238761\n239760\n240759\n241758\n242757\n243756\n244755\n245754\n246753\n247752\n248751\n249750\n250749\n251748\n252747\n253746\n254745\n255744\n256743\n257742\n258741\n259740\n260739\n261738\n262737\n263736\n264735\n265734\n266733\n267732\n268731\n269730\n270729\n271728\n272727\n273726\n274725\n275724\n276723\n277722\n278721\n279720\n280719\n281718\n282717\n283716\n284715\n285714\n286713\n287712\n288711\n289710\n290709\n291708\n292707\n293706\n294705\n295704\n296703\n297702\n298701\n299700\n300699\n301698\n302697\n303696\n304695\n305694\n306693\n307692\n308691\n309690\n310689\n311688\n312687\n313686\n314685\n315684\n316683\n317682\n318681\n319680\n320679\n321678\n322677\n323676\n324675\n325674\n326673\n327672\n328671\n329670\n330669\n331668\n332667\n333666\n334665\n335664\n336663\n337662\n338661\n339660\n340659\n341658\n342657\n343656\n344655\n345654\n346653\n347652\n348651\n349650\n350649\n351648\n352647\n353646\n354645\n355644\n356643\n357642\n358641\n359640\n360639\n361638\n362637\n363636\n364635\n365634\n366633\n367632\n368631\n369630\n370629\n371628\n372627\n373626\n374625\n375624\n376623\n377622\n378621\n379620\n380619\n381618\n382617\n383616\n384615\n385614\n386613\n387612\n388611\n389610\n390609\n391608\n392607\n393606\n394605\n395604\n396603\n397602\n398601\n399600\n400599\n401598\n402597\n403596\n404595\n405594\n406593\n407592\n408591\n409590\n410589\n411588\n412587\n413586\n414585\n415584\n416583\n417582\n418581\n419580\n420579\n421578\n422577\n423576\n424575\n425574\n426573\n427572\n428571\n429570\n430569\n431568\n432567\n433566\n434565\n435564\n436563\n437562\n438561\n439560\n440559\n441558\n442557\n443556\n444555\n445554\n446553\n447552\n448551\n449550\n450549\n451548\n452547\n453546\n454545\n455544\n456543\n457542\n458541\n459540\n460539\n461538\n462537\n463536\n464535\n465534\n466533\n467532\n468531\n469530\n470529\n471528\n472527\n473526\n474525\n475524\n476523\n477522\n478521\n479520\n480519\n481518\n482517\n483516\n484515\n485514\n486513\n487512\n488511\n489510\n490509\n491508\n492507\n493506\n494505\n495504\n496503\n497502\n498501\n499500\n500499\n501498\n502497\n503496\n504495\n505494\n506493\n507492\n508491\n509490\n510489\n511488\n512487\n513486\n514485\n515484\n516483\n517482\n518481\n519480\n520479\n521478\n522477\n523476\n524475\n525474\n526473\n527472\n528471\n529470\n530469\n531468\n532467\n533466\n534465\n535464\n536463\n537462\n538461\n539460\n540459\n541458\n542457\n543456\n544455\n545454\n546453\n547452\n548451\n549450\n550449\n551448\n552447\n553446\n554445\n555444\n556443\n557442\n558441\n559440\n560439\n561438\n562437\n563436\n564435\n565434\n566433\n567432\n568431\n569430\n570429\n571428\n572427\n573426\n574425\n575424\n576423\n577422\n578421\n579420\n580419\n581418\n582417\n583416\n584415\n585414\n586413\n587412\n588411\n589410\n590409\n591408\n592407\n593406\n594405\n595404\n596403\n597402\n598401\n599400\n600399\n601398\n602397\n603396\n604395\n605394\n606393\n607392\n608391\n609390\n610389\n611388\n612387\n613386\n614385\n615384\n616383\n617382\n618381\n619380\n620379\n621378\n622377\n623376\n624375\n625374\n626373\n627372\n628371\n629370\n630369\n631368\n632367\n633366\n634365\n635364\n636363\n637362\n638361\n639360\n640359\n641358\n642357\n643356\n644355\n645354\n646353\n647352\n648351\n649350\n650349\n651348\n652347\n653346\n654345\n655344\n656343\n657342\n658341\n659340\n660339\n661338\n662337\n663336\n664335\n665334\n666333\n667332\n668331\n669330\n670329\n671328\n672327\n673326\n674325\n675324\n676323\n677322\n678321\n679320\n680319\n681318\n682317\n683316\n684315\n685314\n686313\n687312\n688311\n689310\n690309\n691308\n692307\n693306\n694305\n695304\n696303\n697302\n698301\n699300\n700299\n701298\n702297\n703296\n704295\n705294\n706293\n707292\n708291\n709290\n710289\n711288\n712287\n713286\n714285\n715284\n716283\n717282\n718281\n719280\n720279\n721278\n722277\n723276\n724275\n725274\n726273\n727272\n728271\n729270\n730269\n731268\n732267\n733266\n734265\n735264\n736263\n737262\n738261\n739260\n740259\n741258\n742257\n743256\n744255\n745254\n746253\n747252\n748251\n749250\n750249\n751248\n752247\n753246\n754245\n755244\n756243\n757242\n758241\n759240\n760239\n761238\n762237\n763236\n764235\n765234\n766233\n767232\n768231\n769230\n770229\n771228\n772227\n773226\n774225\n775224\n776223\n777222\n778221\n779220\n780219\n781218\n782217\n783216\n784215\n785214\n786213\n787212\n788211\n789210\n790209\n791208\n792207\n793206\n794205\n795204\n796203\n797202\n798201\n799200\n800199\n801198\n802197\n803196\n804195\n805194\n806193\n807192\n808191\n809190\n810189\n811188\n812187\n813186\n814185\n815184\n816183\n817182\n818181\n819180\n820179\n821178\n822177\n823176\n824175\n825174\n826173\n827172\n828171\n829170\n830169\n831168\n832167\n833166\n834165\n835164\n836163\n837162\n838161\n839160\n840159\n841158\n842157\n843156\n844155\n845154\n846153\n847152\n848151\n849150\n850149\n851148\n852147\n853146\n854145\n855144\n856143\n857142\n858141\n859140\n860139\n861138\n862137\n863136\n864135\n865134\n866133\n867132\n868131\n869130\n870129\n871128\n872127\n873126\n874125\n875124\n876123\n877122\n878121\n879120\n880119\n881118\n882117\n883116\n884115\n885114\n886113\n887112\n888111\n889110\n890109\n891108\n892107\n893106\n894105\n895104\n896103\n897102\n898101\n899100\n900099\n901098\n902097\n903096\n904095\n905094\n906093\n907092\n908091\n909090\n910089\n911088\n912087\n913086\n914085\n915084\n916083\n917082\n918081\n919080\n920079\n921078\n922077\n923076\n924075\n925074\n926073\n927072\n928071\n929070\n930069\n931068\n932067\n933066\n934065\n935064\n936063\n937062\n938061\n939060\n940059\n941058\n942057\n943056\n944055\n945054\n946053\n947052\n948051\n949050\n950049\n951048\n952047\n953046\n954045\n955044\n956043\n957042\n958041\n959040\n960039\n961038\n962037\n963036\n964035\n965034\n966033\n967032\n968031\n969030\n970029\n971028\n972027\n973026\n974025\n975024\n976023\n977022\n978021\n979020\n980019\n981018\n982017\n983016\n984015\n985014\n986013\n987012\n988011\n989010\n990009\n991008\n992007\n993006\n994005\n995004\n996003\n997002\n998001\n0",
    },
    {
        name: diamondback_fun_no_args,
        file: "diamondback/fun_no_args.bet",
        expected: "true\nfalse\n0\n-1\n1",
    },
    {
        name: diamondback_calling_chain0,
        file: "diamondback/calling_chain0.bet",
        expected: "100\n100",
    },
    {
        name: diamondback_calling_chain1,
        file: "diamondback/calling_chain1.bet",
        expected: "100\n100",
    },
    {
        name: diamondback_conveyer_belt,
        file: "diamondback/conveyer_belt.bet",
        expected: "-50"
    },
    {
        name: diamondback_decreasing_args,
        file: "diamondback/decreasing_args.bet",
        expected: "12"
    },
    {
        name: diamondback_many_unused_functions,
        file: "diamondback/many_unused_functions.bet",
        input: "42",
        expected: "84",
    },
    // More complex recursive functions
    {
        name: diamondback_recursive_ackermann,
        file: "diamondback/recursive_ackermann.bet",
        expected: "61",
    },
    {
        name: diamondback_recursive_factorial,
        file: "diamondback/recursive_factorial.bet",
        expected: "1\n1\n2\n6\n24\n120\n720\n5040",
    },
    {
        name: diamondback_recursive_fibonacci,
        file: "diamondback/recursive_fibonacci.bet",
        expected: "55",
    },

    // Egg Eater Tests
    {
        name: eggeater_alloc_temp,
        file: "eggeater/alloc_zero_test.bet",
        expected: "0",
    },
    {
        name: eggeater_null_comparison,
        file: "eggeater/null_comparison.bet",
        expected: "false",
    },
    {
        name: eggeater_null_equality,
        file: "eggeater/null_equality.bet",
        expected: "true",
    },
    {
        name: eggeater_recursive_structure,
        file: "eggeater/recursive_structure.bet",
        expected:"1\n2"
    },
    {
        name: eggeater_simple_examples,
        file: "eggeater/simple_examples.bet",
        expected:"1\n2\n2"
    },
    {
        name: eggeater_points,
        file: "eggeater/points.bet",
        expected:"4\n16\n6\n18\n8\n20"
    },
    {
        name: eggeater_bst,
        file: "eggeater/bst.bet",
        expected:"4\n3\n7\n1\nnull pointer to struct bst\n5\n8"
    },
    
    // Final tests
    {
        name: final_useless_parens,
        file: "final/parens.bet",
        expected: "69"
    },
    {
        name: final_unit_parens,
        file: "final/parens_unit.bet",
        expected: "unit"
    },
    {
        name: final_basic_comment,
        file: "final/basic_comment.bet",
        expected:"5"
    },
    {
        name: final_double_comment,
        file: "final/double_comment.bet",
        expected:"5"
    },
    {
        name: final_escaped_comment,
        file: "final/escaped_comment.bet",
        expected:"5"
    },
    {
        name: final_inline_comment,
        file: "final/inline_comment.bet",
        expected:"5"
    },
    {
        name: final_special_comment,
        file: "final/special_comment.bet",
        expected:"5"
    },
    {
        name: final_not_true,
        file: "final/not_true.bet",
        expected:"false"
    },
    {
        name: final_not_false,
        file: "final/not_false.bet",
        expected:"true"
    },
    {
        name: final_not_composite,
        file: "final/not_composite.bet",
        expected:"true"
    },
    {
        name: final_not_not,
        file: "final/not_not.bet",
        expected:"true"
    },
    {
        name: final_or_tt,
        file: "final/or_tt.bet",
        expected:"true"
    },
    {
        name: final_or_tf,
        file: "final/or_tf.bet",
        expected:"true"
    },
    {
        name: final_or_ft,
        file: "final/or_ft.bet",
        expected:"true"
    },
    {
        name: final_or_ff,
        file: "final/or_ff.bet",
        expected:"false"
    },
    {
        name: final_or_composite,
        file: "final/or_composite.bet",
        expected:"true"
    },
    {
        name: final_or_if,
        file: "final/or_if.bet",
        expected:"1"
    },
    {
        name: final_and_tt,
        file: "final/and_tt.bet",
        expected:"true"
    },
    {
        name: final_and_tf,
        file: "final/and_tf.bet",
        expected:"false"
    },
    {
        name: final_and_ft,
        file: "final/and_ft.bet",
        expected:"false"
    },
    {
        name: final_and_ff,
        file: "final/and_ff.bet",
        expected:"false"
    },
    {
        name: final_and_composite,
        file: "final/and_composite.bet",
        expected:"false"
    },
    {
        name: final_and_if,
        file: "final/and_if.bet",
        expected:"0"
    },
    {
        name: final_de_morgans,
        file: "final/de_morgans.bet",
        expected:"true\ntrue\ntrue\ntrue"
    },
    {
        name: final_unit_block,
        file: "final/unit_block.bet",
        expected: "unit"
    },
    {
        name: final_unit_fn,
        file: "final/unit_fn.bet",
        expected: "unit"
    },
    {
        name: final_unit_print,
        file: "final/unit_print.bet",
        expected: "1\nunit\n2\nunit\n3\nunit"
    },
    {
        name: final_unit_struct,
        file: "final/unit_struct.bet",
        expected: "unit"
    },
    {
        name: final_unit_long_block,
        file: "final/unit_long_block.bet",
        expected: "unit"
    },
    {
        name: final_unit_let,
        file: "final/unit_let.bet",
        expected: "unit"
    },
    {
        name: final_unit_call,
        file: "final/unit_call.bet",
        expected: "unit"
    },
    //// function pointer tests
    {
        name: final_fun_ptr_unit_sig,
        file: "final/fun_ptr_unit.bet",
        expected: "1\n2\n3\nunit"
    },
    {
        name: final_fun_ptr_int_sig,
        file: "final/fun_ptr_int.bet",
        expected: "true\n1\n2\ntrue\nfalse\n1\n2\nfalse\n3\n3"
    },
    {
        name: final_fun_ptr_struct,
        file: "final/fun_ptr_struct.bet",
        expected: "6"
    },
    {
        name: final_arr_len1,
        file: "final/arr_basic.bet",
        input: "10",
        expected: "unit",
    },
    {
        name: final_arr_len2,
        file: "final/arr_basic.bet",
        input: "1000",
        expected: "unit",
    },
    {
        name: final_arr_lookup1,
        file: "final/arr_lookup.bet",
        input: "0",
        expected: "0",
    },
    {
        name: final_arr_lookup2,
        file: "final/arr_lookup.bet",
        input: "9",
        expected: "0",
    },
    {
        name: final_arr_map1,
        file: "final/arr_map.bet",
        input: "9",
        expected: "18",
    },
    {
        name: final_arr_map2,
        file: "final/arr_map.bet",
        input: "2",
        expected: "4",
    },
    {
        name: final_arr_arr1,
        file: "final/arr_arr.bet",
        input: "10",
        expected: "6\n5",
    },
    {
        name: final_arr_arr2,
        file: "final/arr_arr.bet",
        input: "100",
        expected: "96\n95",
    },
    {
        name: final_arr_struct1,
        file: "final/arr_struct.bet",
        input: "3",
        expected: "0\n1\n2\nunit",
    },
    {
        name: final_arr_struct2,
        file: "final/arr_struct.bet",
        input: "1",
        expected: "0\nunit",
    },
    {
        name: final_triple_arr,
        file: "final/arr_tripple_dept.bet",
        expected: "420",
    },
}

runtime_error_tests! {
   {
        name: cobra_too_negative_input,
        file: "cobra/just_input.bet",
        input: "-9223372036854775809",
        expected: "Invalid Input",
    },
    {
        name: cobra_too_positive_input,
        file: "cobra/just_input.bet",
        input: "9223372036854775808",
        expected: "Invalid Input",
    },

    ///// New-to-Diamondback Auto Grader
    // integer overflow
    {
        name: diamondback_number_overflow_fail2,
        file: "diamondback/add.bet",
        input: "9223372036854775803",
        expected: "overflow",
    },
    {
        name: diamondback_number_overflow_fail3,
        file: "diamondback/nested_arith3.bet",
        input: "9223372036854775794",
        expected: "overflow",
    },

    // New to Eggeater
    {
        name: eggeater_alloc_out_of_space_fail,
        file: "eggeater/allocator_overflow_error_fail.bet",
        expected: "out of space",
    },
    {
        name: eggeater_update_null_dereference_fail,
        file: "eggeater/update_null_dereference_fail.bet",
        expected: "null dereference",
    },
    {
        name: eggeater_lookup_null_dereference_fail,
        file: "eggeater/lookup_null_dereference_fail.bet",
        expected: "null dereference",
    },
    {
        name: eggeater_update_null_fail,
        file: "eggeater/update_null_fail.bet",
        expected: "null dereference",
    },
    {
        name: eggeater_lookup_null_fail,
        file: "eggeater/lookup_null_fail.bet",
        expected: "null dereference",
    },
    //// trying to call a null-function pointer at runtime results in a runtime error
    {
        name: final_fun_ptr_null_fail,
        file: "final/fun_ptr_null_deref.bet",
        expected: "null dereference",
    },
    {
        name: final_neg_arr_len,
        file: "final/arr_basic.bet",
        input: "-1",
        expected: "invalid array size",
    },
    {
        name: final_zero_arr_len,
        file: "final/arr_basic.bet",
        input: "0",
        expected: "invalid array size",
    },
    {
        name: final_huge_arr_len,
        file: "final/arr_basic.bet",
        input: "100000000",
        expected: "out of space",
    },
    {
        name: final_neg_lookup,
        file: "final/arr_lookup.bet",
        input: "-1",
        expected: "invalid array access",
    },
    {
        name: final_huge_lookup,
        file: "final/arr_lookup.bet",
        input: "19",
        expected: "invalid array access",
    },
    
}

static_error_tests! {
    {
        name: diamondback_set_input,
        file: "diamondback/input_set_fail.bet",
        expected: "keyword"
    },

    {
        name: cobra_invalid_argument,
        file: "cobra/invalid_argument.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_add1,
        file: "cobra/invalid_argument_add1.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_greater,
        file: "cobra/invalid_argument_greater.bet",
        expected: "mismatch",
    },
        {
        name: cobra_invalid_argument_equal,
        file: "cobra/invalid_argument_equal.bet",
        expected: "mismatch",
    },
    {
        name: cobra_number_bounds_fail,
        file: "cobra/number_bounds_fail.bet",
        expected: "Invalid",
    },
    {
        name: cobra_failed_set,
        file: "cobra/failed_set.bet",
        expected: "mismatch",
    },
    {
        name: diamondback_duplicate_params,
        file: "diamondback/duplicate_params.bet",
        expected: "Duplicate Argument",
    },

    // Our Egg Eater Tests
    {
        name: eggeater_duplicate_fun_struct,
        file: "eggeater/duplicate_fun_struct_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_alloc_nonexistent_struct,
        file: "eggeater/alloc_nonexistent_struct_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_duplicate_struct,
        file: "eggeater/duplicate_struct_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_duplicate_field,
        file: "eggeater/duplicate_field_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_empty_struct,
        file: "eggeater/empty_struct_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_if_type_mismatch,
        file: "eggeater/if_type_mismatch_fail.bet",
        expected: "mismatch",
    },
    {
        name: eggeater_invalid_update_field,
        file: "eggeater/invalid_update_field_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_lookup_nonexistent,
        file: "eggeater/lookup_nonexistent_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_lookup_nonpointer,
        file: "eggeater/lookup_nonpointer_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_invalid_field_type,
        file: "eggeater/invalid_field_type_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_null_lookup_compile,
        file: "eggeater/null_lookup_comp_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_bad_null,
        file: "eggeater/set_null_fail.bet",
        expected: "keyword",
    },
    {
        name: eggeater_struct_null,
        file: "eggeater/set_struct_fail.bet",
        expected: "mismatch",
    },
    {
        name: eggeater_null_int_ptr,
        file: "eggeater/null_int_ptr.bet",
        expected: "keyword",
    },

    {
        name: eggeater_update_nonexistent,
        file: "eggeater/update_nonexistent_fail.bet",
        expected: "Invalid",
    },
    {
        name: eggeater_update_nonpointer,
        file: "eggeater/update_nonpointer_fail.bet",
        expected: "Invalid",
    },
    ///// New-to-Cobra Auto Grader

    // Type errors
    {
        name: cobra_if_expr_succ0,
        file: "cobra/if_expr_succ0.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail8,
        file: "cobra/if_expr_input.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail0,
        file: "cobra/invalid_argument_fail0.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail1,
        file: "cobra/invalid_argument_fail1.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail2,
        file: "cobra/invalid_argument_fail2.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail3,
        file: "cobra/invalid_argument_fail3.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail4,
        file: "cobra/invalid_argument_fail4.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail6,
        file: "cobra/invalid_argument_fail6.bet",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail11,
        file: "cobra/invalid_argument_fail11.bet",
        expected: "mismatch",
    },
    // Invalid S-expressions
    {
        name: cobra_parse_sexp_fail2,
        file: "cobra/parse_sexp_fail2.bet",
        expected: "Invalid",
    },

    // Invalid tokens/operators
    {
        name: cobra_parse_token_fail1,
        file: "cobra/parse_token_fail1.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_token_fail2,
        file: "cobra/parse_token_fail2.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_token_fail3,
        file: "cobra/parse_token_fail3.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_token_fail4,
        file: "cobra/parse_token_fail4.bet",
        expected: "Invalid",
    },


    // Invalid/Out of bounds Number Literal
    {
        name: cobra_number_bounds_fail0,
        file: "cobra/number_bounds_fail0.bet",
        expected: "Invalid",
    },
    {
        name: cobra_number_bounds_fail1,
        file: "cobra/number_bounds_fail1.bet",
        expected: "Invalid",
    },

    // Invalid operator arguments
    {
        name: cobra_parse_op_fail1,
        file: "cobra/parse_op_fail1.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail3,
        file: "cobra/parse_op_fail3.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fai4,
        file: "cobra/parse_op_fail4.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail6,
        file: "cobra/parse_op_fail6.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail7,
        file: "cobra/parse_op_fail7.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail8,
        file: "cobra/parse_op_fail8.bet",
        expected: "Invalid",
    },

    // Invalid let expressions
    {
        name: cobra_parse_let_nobindings_fail,
        file: "cobra/parse_let_nobindings_fail.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail3,
        file: "cobra/parse_let_improperargs_fail3.bet",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail5,
        file: "cobra/parse_let_improperargs_fail5.bet",
        expected: "keyword",
    },

    {
        name: boa_duplicate_binding_fail0,
        file: "boa/duplicate_binding_fail0.bet",
        expected: "Duplicate binding",
    },
    {
        name: boa_duplicate_binding_fail1,
        file: "boa/duplicate_binding_fail1.bet",
        expected: "Duplicate binding",
    },
    {
        name: boa_duplicate_binding_fail2,
        file: "boa/duplicate_binding_fail2.bet",
        expected: "Duplicate binding",
    },

    // Invalid if expressions
    {
        name: cobra_parse_if_fail1,
        file: "cobra/parse_if_fail1.bet",
        expected: "Invalid",
    },

    // Unbound identifier
    {
        name: cobra_mul_over,
        file: "cobra/mul_over.bet",
        expected: "overflow",
    },
    {
        name: cobra_unbound_identifier_fail0,
        file: "cobra/unbound_identifier_fail0.bet",
        expected: "Unbound variable identifier x",
    },
    {
        name: cobra_unbound_identifier_fail1,
        file: "cobra/unbound_identifier_fail1.bet",
        expected: "Unbound variable identifier y",
    },
    {
        name: cobra_unbound_identifier_fail2,
        file: "cobra/unbound_identifier_fail2.bet",
        expected: "Unbound variable identifier x",
    },
    {
        name: cobra_unbound_identifier_fail3,
        file: "cobra/unbound_identifier_fail3.bet",
        expected: "Unbound variable identifier z",
    },
    {
        name: cobra_unbound_identifier_fail4,
        file: "cobra/unbound_identifier_fail4.bet",
        expected: "Unbound variable identifier t",
    },
    {
        name: cobra_unbound_identifier_fail5,
        file: "cobra/unbound_identifier_fail5.bet",
        expected: "Unbound variable identifier x",
    },

    // Invalid block
    {
        name: cobra_parse_block_fail0,
        file: "cobra/parse_block_fail0.bet",
        expected: "Invalid",
    },

    // Invalid break
    {
        name: cobra_invalid_break_fail0,
        file: "cobra/invalid_break_fail0.bet",
        expected: "break",
    },

    // Invalid loop
    {
        name: cobra_invalid_loop_fail0,
        file: "cobra/invalid_loop_fail0.bet",
        expected: "Invalid",
    },
    
    // Final
    {
        name: final_not_invalid,
        file: "final/not_invalid.bet",
        expected:"Invalid"
    },
    {
        name: final_not_mismatch,
        file: "final/not_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_or_invalid,
        file: "final/or_invalid.bet",
        expected:"Invalid"
    },
    {
        name: final_or_invalid2,
        file: "final/or_invalid2.bet",
        expected:"Invalid"
    },
    {
        name: final_left_or_mismatch,
        file: "final/left_or_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_right_or_mismatch,
        file: "final/right_or_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_both_or_mismatch,
        file: "final/both_or_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_and_invalid,
        file: "final/and_invalid.bet",
        expected:"Invalid"
    },
    {
        name: final_and_invalid2,
        file: "final/and_invalid2.bet",
        expected:"Invalid"
    },
    {
        name: final_left_and_mismatch,
        file: "final/left_and_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_right_and_mismatch,
        file: "final/right_and_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_both_and_mismatch,
        file: "final/both_and_mismatch.bet",
        expected:"mismatch"
    },
    {
        name: final_arr_wrong_elem_type,
        file: "final/arr_wrong_elem_type.bet",
        expected:"mismatch"
    },
    {
        name: final_arr_non_int_ind,
        file: "final/arr_non_int_ind.bet",
        expected:"mismatch"
    },
    {
        name: final_arr_bad_read,
        file: "final/arr_read_fail.bet",
        expected: "Invalid"
    },
    {
        name: final_arr_bad_type,
        file: "final/arr_bad_type.bet",
        expected: "Unrecognized"
    },
    {
        name: final_arr_arr_bad_type,
        file: "final/arr_arr_bad_type.bet",
        expected: "Unrecognized"
    },
    {
        name: final_arr_bad_len,
        file: "final/arr_bad_len.bet",
        expected: "mismatch"
    },
    {
        name: final_arr_non_int_write,
        file: "final/arr_non_int_write.bet",
        expected: "mismatch"
    },
    {
        name: final_new_arr_missing_arg,
        file: "final/new_arr_missing.bet",
        expected: "Parse Error"
    },
    {
        name: final_new_arr_extra_arg,
        file: "final/new_arr_extra.bet",
        expected: "Parse Error"
    },
}
