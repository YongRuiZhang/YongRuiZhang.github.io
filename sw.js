/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","9472287b2eb1bfcc7eac3f8678a8e040"],["/about/index.html","bb536ae998274b812b3627836ec2dba8"],["/archives/2023/01/index.html","0339a78d58d21a7eb955615839bc618a"],["/archives/2023/02/index.html","ce657e6d442baaf34f1334a968d0684c"],["/archives/2023/02/page/2/index.html","c56efadb8aa0677d8523efe301a3b1ca"],["/archives/2023/03/index.html","6fbba01c931a5a224ea7b39fa599ee47"],["/archives/2023/05/index.html","b7b3710633af3659bd8939a4ec350a54"],["/archives/2023/06/index.html","9e31ec63e0eb828e15a252c5b2ee3f1f"],["/archives/2023/09/index.html","819c0b17c0f38774b1419729d731cbea"],["/archives/2023/11/index.html","631980170136938bb0a21902fa66d0cb"],["/archives/2023/12/index.html","d1aec9c58cdabcabd2b219bfc93fff0b"],["/archives/2023/index.html","871a61acfc6153ed4b47a97e966869f5"],["/archives/2023/page/2/index.html","4e6f1e6c348bb80b48a27e7a8df11e9d"],["/archives/2023/page/3/index.html","104d2dcf9f4ffe7657e384b99981db6c"],["/archives/2023/page/4/index.html","0d04cccf4a748883a782e063fe1f0d96"],["/archives/2024/02/index.html","f002bb1b004ca60fa59ee42873a8e1da"],["/archives/2024/index.html","0917a090b943963b34a0a282f71007db"],["/archives/index.html","b580f71ba71cd305b98c0b263a0c1c09"],["/archives/page/2/index.html","5777647214c628d132045649995040f4"],["/archives/page/3/index.html","083a321011f247371ca3932d98f17bb5"],["/archives/page/4/index.html","1e7e3835a820b5e4ccb209a2952da682"],["/baidu_verify_codeva-qQP2iZOMLX.html","d1478452b9fa80583adcaa8ebffb4abd"],["/categories/Java/index.html","2e5e20e5ca5caa66d2ef57b25a6c1b5c"],["/categories/Java/后端/index.html","e9caa01e466883aee90cf8029df8d250"],["/categories/Java/基础/index.html","7464b50ab1dedf559f55b53e073c617b"],["/categories/Java/基础/集合/index.html","cc6e5690d85f09fae76b3db84ef07c6c"],["/categories/Python/index.html","5a4c7ebe221a0c8e4320e6408e4e8c82"],["/categories/Python/编程环境/index.html","068043fc027a9be7cc27f0a8c632a885"],["/categories/R语言/index.html","46be2b4dd5cb487203772e7d0baa1e1c"],["/categories/R语言/编程环境/index.html","c46769cb57f5ac21a6f2ecb9a61445c7"],["/categories/index.html","3b078a1fd09fa4239f9fff234a1dfa46"],["/categories/中间件/index.html","280eb9d617684fd509537fb9f73e6439"],["/categories/前端/Vue/index.html","64f5764c5d3415c62b3e5cf90f05154d"],["/categories/前端/index.html","aadcefde6f812e9d5bad9ebd4582d0c7"],["/categories/大数据开发/ElasticSearch/index.html","2ab17d716394e29bd1667218a05c9e1b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","200bbd87d31cbe04cd0e7c0ae9d515d7"],["/categories/大数据开发/HBase/index.html","910018b38b10db1d273a8ff57d13c37b"],["/categories/大数据开发/HBase/学习笔记/index.html","bdaa5a3f973ea026d90227a13da61fbf"],["/categories/大数据开发/HBase/环境搭建/index.html","ea2f21b68732fd9b29cb9e6db1060a81"],["/categories/大数据开发/Hadoop/index.html","782e0e94b24598f59c1cecc9a1d5802d"],["/categories/大数据开发/Hadoop/技术/index.html","04c63cb302126a15aed5350eabd96ac0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","be54968d45e5e24f451405f0aa477b52"],["/categories/大数据开发/Redis/index.html","62e7dcc4fd72768894583bc4f16f54ad"],["/categories/大数据开发/Redis/技术/index.html","ee019a185c0728e79d5c71f58e70ee49"],["/categories/大数据开发/Redis/环境搭建/index.html","2fc5e56e69a8aa2b30ddebd4fdfd2463"],["/categories/大数据开发/Spark/index.html","23f0eb0ad2124df4f13967e26fb47c8f"],["/categories/大数据开发/Spark/环境搭建/index.html","4b720f8e7cc7bcb5dab0981c08996349"],["/categories/大数据开发/Zookeeper/index.html","ae82dd874b4d9f37874f17585c17f712"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","966954305c33a0e0f8fbca4b1aec449c"],["/categories/大数据开发/index.html","321ec4a519ce81c5acb6b5e29798b9c9"],["/categories/学校课程/index.html","0ad4eeda54699fd6fb019e2da92c61a8"],["/categories/学校课程/计算机操作系统/index.html","95ce994a2e47af614b10f1e23b425495"],["/categories/操作系统/Linux/index.html","612a7c45b9e59c191d62423bc7a5e009"],["/categories/操作系统/Mac/index.html","a03698795da9f7a9780dd093b2f889cf"],["/categories/操作系统/Windows/index.html","6d21b97ad0e8e8063a4c013d5c2553f0"],["/categories/操作系统/index.html","c06ac14fe34ce4ad6b387190b45870be"],["/categories/数学建模/index.html","cfe56685d82adc2562d33a6829c23420"],["/categories/数学建模/latex/index.html","1cc062f987fb2533dd528a7faa544829"],["/categories/数学建模/优化类/index.html","fa0b5f1491463664f71b8c324094ffb5"],["/categories/数学建模/优化类/现代优化算法/index.html","584247a6c6742133aed4db1b3afa5843"],["/categories/数学建模/优化类/规划类/index.html","f8b0e38ce00c6a2ffd225c1960c4c7e0"],["/categories/数学建模/绘图/index.html","d9f7a6c68cbb9a04379a2a8cb78ff169"],["/categories/数据库/MySQL/index.html","bf60ebbdb38af1f2e88b96938e6d89ed"],["/categories/数据库/index.html","a74ac4de2daca1c9b3a929724711115f"],["/categories/数据结构和算法/index.html","2d6f799c7499bcd1aa3af0b3ba07820a"],["/categories/数据结构和算法/page/2/index.html","6bc308b49334d1919a3e206f2afd1237"],["/categories/数据结构和算法/基本原理/bfs/index.html","1fef344bc627c9b41fb0062c9165b956"],["/categories/数据结构和算法/基本原理/dfs/index.html","bf7f1a75c781c1b2140d76935690dcff"],["/categories/数据结构和算法/基本原理/index.html","f6c63002e3ebdf61396941283cac9d92"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9ebbdd6906daf73c177433123fa2dd21"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e6a0c66d8557e3b52f91e6de650082d7"],["/categories/数据结构和算法/基本原理/图论/index.html","a2805e967ab08ee2315742890b385453"],["/categories/数据结构和算法/基本原理/字符串/index.html","282371486df0b0af384ed00f3c10b967"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","3440154167ed730f7e20bf6cd2cf6fb8"],["/categories/数据结构和算法/基本原理/数论/index.html","25aa2604f3e024a59837ac4aa024f032"],["/categories/数据结构和算法/基本原理/树论/index.html","56f7ced4e09638152411d8448c74942f"],["/categories/数据结构和算法/基本原理/链表/index.html","c8d72c41185bf4d7fa60074d68bb0a0d"],["/categories/数据结构和算法/算法题/index.html","d31f5114b1e4193692246a14460bafb8"],["/categories/数据结构和算法/算法题/二分查找/index.html","1f3250075b989c48909ad4200855b24e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","279f5050ae732be78b69945835e24d90"],["/categories/数据结构和算法/算法题/动态规划/index.html","f95c6bc8c5723e68bb47e6af54d1b220"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","22bbd316768f32147d4720d866b1f504"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3e2252ef51fff18d3142dfe89e9ef527"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","0e2f227468b48cac3d741dde70541482"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2af55ec7f05bdaf0f185f845ed42ef42"],["/categories/数据结构和算法/算法题/数论/index.html","2468244be00ac6ffd3e0e0f52fdaddc1"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1ebaf01e9c6d7bc43a4a0e707bc1591b"],["/categories/数据结构和算法/算法题/树论/index.html","f5e4044c270c3e9e7b9e5bcba834c7ff"],["/categories/杂七杂八/index.html","ea94f2bff5736c19ace3a3888188ad27"],["/categories/杂七杂八/博客搭建/index.html","621b83c381205ca4315291b2dbed63be"],["/categories/编程工具下载/index.html","b4fa31d8857bb87bee4fabbce470c69a"],["/categories/编程环境/index.html","fce738e5d1a12a152c5810cba1fdfa8f"],["/categories/编程环境/大数据/index.html","5151d351e3a24c5074cf2840952e8862"],["/categories/英语学习/index.html","42a7c5d255c26423290e2a8c2acb0727"],["/categories/英语学习/英语语法/index.html","c233762a99a4ae9d253790be07c19061"],["/comments/index.html","c3dc040155757bdad67e9de0e129ce1c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2633584012071be9fb20499766ec50a3"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a68ddbe49b545333dfde106ec1396bdb"],["/movies/index.html","cf0c0d20e4126c01dbf474d0f8f30b1d"],["/music/index.html","70c909e2248f68e0f55168c5c16091a4"],["/page/2/index.html","d8ea819881a10928f5429650d6adb717"],["/page/3/index.html","5da86ecb635a80bd136e45a0cf29621b"],["/page/4/index.html","56aa346564a95b55210c2f4a3c91e6fa"],["/page/5/index.html","78e4776d76da8339daa205059556aa27"],["/page/6/index.html","80117a42c063f874bf04f517c10ff866"],["/posts/1021360842.html","539e373ceb4d530b1a90d5d3a738bfa1"],["/posts/1120620192.html","feda187e5c571ce627eab822f0cf436d"],["/posts/1141628095.html","c8a934b6953dfdfe6cb6dc2899f62cb1"],["/posts/1168613674.html","371fbef71d988b4d651db5a9065799e3"],["/posts/1219920510.html","84854a9d4eb3a1ae2b83a231a217d5b9"],["/posts/1222166338.html","dd14eeacdde59a9b5f0c6515eabc0ed2"],["/posts/1259097482.html","2c6c853a5da18962ceb1283b81307535"],["/posts/1271036369.html","bcb474530527207847114542376a57b0"],["/posts/1312847445.html","7c21b184d0829cbc78564aa380b82a40"],["/posts/135355774.html","a300a806b67eae685c549597f9258b1e"],["/posts/1375344716.html","c9cd0795dff208955c1b8bc9adb09629"],["/posts/1388991698.html","ca26dadb53b7424ddef92d77f0b0981e"],["/posts/1410315814.html","4d3bf07840a1de00b8e14758bee72465"],["/posts/1452790229.html","33a8c6da9c93d8c8f2e33f7c509d7ae2"],["/posts/1470079884.html","9cb686efd1c105fd2e843c7081bef2d3"],["/posts/1470079885.html","03dc440f04759216f4f9a726fdd32874"],["/posts/1470079886.html","6e7ce686a2e4c4f1f0d6006fdca9ce1f"],["/posts/1470079887.html","8825ece01ede3b1172b260a60b3f8b64"],["/posts/1498536549.html","5e723b45bcbb9b43aba3a49d275c4cf1"],["/posts/1539568593.html","01a5618c06bb5e695554300449fdef19"],["/posts/1547067935.html","530e636ac030f6f0c859ae9b74f0bdd0"],["/posts/1557866301.html","51bbaae827a1bef35ddd5f9ae23dbe23"],["/posts/1571776361.html","45a4930024ff09674ca2da9d5c28a04e"],["/posts/1605124548.html","944f0f8bfaff088604902c99d0c549c8"],["/posts/1633036852.html","c197454d8bd75170f4dffc5077db5795"],["/posts/1674202625.html","566543b4818391f54e7bf768ec217c5a"],["/posts/1765123828.html","007d6b7856637e66f9639b2c9887adfc"],["/posts/1767336200.html","e807a7340e7b2b1b4c414eac66ecbbeb"],["/posts/1776114197.html","47a14930000d50dc977cd048949c25d3"],["/posts/1817748743.html","1a44a0a4515e0acca69de7468e5a02a2"],["/posts/1925125395.html","f9198666f96af57d9b642e392135fc3c"],["/posts/1966191251.html","1383c869e9b929147876df15c90f3490"],["/posts/1987617322.html","bec5f5ebe32647b96fff820358cad40d"],["/posts/1999788039.html","0bf9622b1ed7e2965160aff5f3b5e801"],["/posts/2075104059.html","6e379197c9b028cd70336c735c57788c"],["/posts/2087796737.html","3ce35ef3698208a9c77c8249fd5a7f9c"],["/posts/2106547339.html","2c145b35e5aa7f276bf57c18c7ec58f3"],["/posts/2207806286.html","0ce058f74bedea47330d29ab6f31204e"],["/posts/2225903441.html","c0b6f75ae990c19a2fbc7849a30ef6c8"],["/posts/2265610284.html","4d4883b3f269220e45f7f35bd90e0009"],["/posts/2281352001.html","3555eae20fd17f075ec07dffa67a0251"],["/posts/2364755265.html","2d3b18e704858ff6fb64ca627779bfff"],["/posts/2414116852.html","a1b89ebecbc77f399bab482e110d93c0"],["/posts/2421785022.html","95dc0380ffa1064d0234b080b1b7054a"],["/posts/2482902029.html","cd6bd5929c07fbf87854d27a96d825b9"],["/posts/2495386210.html","389ec8c44ac3b98a280d4bd836130e9b"],["/posts/2516528882.html","c93991d736b4f71fa2f15fd52ffeee0f"],["/posts/2526659543.html","7a639a329fa04cf17e1964e113788192"],["/posts/2529807823.html","6f1c55f3fee995beacedac60c7e4c514"],["/posts/2596601004.html","ac358a80110ad2fde49c5235bab79891"],["/posts/2697614349.html","a7e7a931f11294ba8c8014817401342e"],["/posts/2742438348.html","caa82b6820f3df80eb700409dfdd6e2e"],["/posts/2768249503.html","9e5503a95837b810cbbfa137637805b0"],["/posts/2864584994.html","d17b5219e26167ddd52c1488454cb3a5"],["/posts/2888309600.html","a8289b7c429854116c4ba5fea2220b98"],["/posts/2891591958.html","73206e00c3e861283212c14f494d9f47"],["/posts/2909934084.html","6ef1204a8844a03d29fc065aa48c2260"],["/posts/2920256992.html","74b5258f3faeeb4590f731f288207a6b"],["/posts/2959474469.html","1a01b98f45e4847dcb8b34f41182d9ee"],["/posts/3005926051.html","ec44f4a793053835d4f0d0e9aa4b7a47"],["/posts/309775400.html","d21607620ff1c03372d5e7dbf8f74be7"],["/posts/3156194925.html","3043ba465eebc318cedb15765e3c4dec"],["/posts/3169224211.html","86b9664fbebd3ce0348937c7e0875c15"],["/posts/3213899550.html","c1e98cfbd02bc2aa5d2c4fb8d2603b92"],["/posts/3259212833.html","3568ec21fba1c89d20619ab0f91f91c7"],["/posts/3266130344.html","c72194e71d26588ac6b81818ea5f5b78"],["/posts/3292663995.html","f016f21dc4907484cad6d933837c8c3e"],["/posts/3297135020.html","8106ee20ee2e0798346c6cfd058344d2"],["/posts/3306641566.html","b3963ea2cf319a6162d97dd3683aa62b"],["/posts/3312011324.html","cf8a0565d3babc52eb6f93fb8fd402d7"],["/posts/336911618.html","fe34cc0f149064214d91b215af3f10b8"],["/posts/3402121571.html","0bbb16f15007a5601789ab9d291ede05"],["/posts/3405577485.html","b24070f7659ac6776a1072ff2b5b7e71"],["/posts/3498516849.html","16867d555fcdadcd66540fb6bbf0992d"],["/posts/3513711414.html","220c71c30d9d1b422dd7f6a58bf72baa"],["/posts/3523095624.html","8e236b8d5be7c381ab938db75cc2fc72"],["/posts/3546711884.html","e0fcc45956ca33c084ee5037a789dad2"],["/posts/3731385230.html","60aa39765465797414e72297dd8b9c41"],["/posts/3772089482.html","91fd5ca70cb1117e040b8606ce8d96ba"],["/posts/386609427.html","dadde9e4a380c2a6e010e5161dea5787"],["/posts/4044235327.html","2fb90896e71333a52ead24b28a80cd05"],["/posts/4115971639.html","ad8059d014b5b49fc11ae0d729c3403f"],["/posts/4130790367.html","aedf8cd45dcdd651b3dfe548013fd56a"],["/posts/4131986683.html","92cb517808d707161813a6eeaa700307"],["/posts/4177218757.html","325e59dbf278927c6174a09f87d87dbb"],["/posts/4192183953.html","013a8a7837520da99166839f267be18d"],["/posts/4261103898.html","17d4508efd93acd93126555791e0766b"],["/posts/469711973.html","4f5fd0e89cd14566a099cdd15d27d939"],["/posts/482495853.html","e786ec2cb40c68ecf7ee264f3d06cc21"],["/posts/488247922.html","fc8ccce96f0725b36b85718d46cc8c1f"],["/posts/517302816.html","9efeab03f7f87fb1c956f02b35232210"],["/posts/570165348.html","cb0ea18f9062d009c4bcb2295f1ab1c2"],["/posts/595890772.html","92319afa6ce22f8f9ad1f631570dc018"],["/posts/67485572.html","e60507a17d977cb50be84bd149799bf4"],["/posts/694347442.html","42fa916600dd25213dfd9fbd09436a60"],["/posts/707384687.html","5b63c32235a5dbfcae1e304859111cb4"],["/posts/71180092.html","04e21965ce5156ae1a90e3e4281cf81e"],["/posts/716459272.html","179a76fe2df184ad422d9d28bf7db0b1"],["/posts/765481613.html","05ed807d5cf798644487ac280d83358b"],["/posts/778231993.html","1adab556d6eea868c32a18bbaa34d492"],["/posts/795397410.html","3a8f93dc6adc2abe12c3cd1d9a616202"],["/posts/820223701.html","2cf30ac4703c8a7c10be1a2836b58087"],["/posts/830372185.html","230a989d1277587bc703fba88963472d"],["/posts/88294277.html","f5b1dfba302bb470885307b5e36b97c6"],["/posts/939963535.html","e6be04d39c27a87e0cec5a240075fd4b"],["/posts/983786067.html","74c3ccbf2a75efaf065a62c74aaa2df9"],["/sw-register.js","4c697d0d48e07a808009ef273f3289c0"],["/tags/C/index.html","df0b793f0d792e5b54dc15563db16391"],["/tags/C/page/2/index.html","392f1e6cafdb320bcb1fef490ac58598"],["/tags/C/page/3/index.html","4612101c311743cae8a6998ac62d5c02"],["/tags/C/page/4/index.html","1f420d296b380b995f3e4a3d90760d55"],["/tags/ETL/index.html","d8cf323d667b6616bd9e9648f0d1961f"],["/tags/ElasticSearch/index.html","72b25dac694eae9598dd530e20a4e4ef"],["/tags/GUI/index.html","ea6d7078110ec63a02b74b4770418665"],["/tags/HBase/index.html","a6d56663bc21b4e5e1133e7a557856c2"],["/tags/Hadoop/index.html","892541eab3e62c60c7c87f34a1d1662f"],["/tags/Hadoop/page/2/index.html","99b5a37eac76cbf2082e1ddfd38c93a0"],["/tags/Java/index.html","e3daa881488485058273d7d2e6d0c5ec"],["/tags/Java后端/index.html","e912a7e4616dde2e09616021ea25570b"],["/tags/Java后端/page/2/index.html","95923a3c7c203d994e771b9dfb97cdd4"],["/tags/Java基础/index.html","2927ff88767e5391495d5f884514e502"],["/tags/Java基础/page/2/index.html","7d185fd90510a166e2f8195530780ff0"],["/tags/Kettle/index.html","8df6b5de23df7b4af6d982e8cf0149eb"],["/tags/Kibana/index.html","4602fd7f9f1e466c8d9af3c341f3a7fa"],["/tags/Linux/index.html","fb4b9728f7d6f9b130999daa2b1323e7"],["/tags/Linux/page/2/index.html","7ae801331b18cb66aaaafda7230588e5"],["/tags/Linux/page/3/index.html","d3f0a5270aeb46cabc4856ea7485d7a4"],["/tags/Mac/index.html","cc14d045be625181aa52dd052882f927"],["/tags/Mac/page/2/index.html","d28956dff1ccbadb7fe2d76e715e8503"],["/tags/Maven/index.html","5471b71c742683a663797d6e0fa588ff"],["/tags/MySQL/index.html","ca5ae66afcb5fdc4447dc8f57bc37152"],["/tags/Python/index.html","6301df7449b1112916aca1c34a630647"],["/tags/Redis/index.html","d9fb057daceaddb2632c013e38f91c1a"],["/tags/R语言/index.html","62040c005cc5a7b7416effb77af1df77"],["/tags/Spark/index.html","8428dadfc09057735ee767f0eb07887e"],["/tags/Ubuntu/index.html","42cf41ddef3a3d78925cb04301b2c493"],["/tags/Vue/index.html","3316cdce485519cdab4b7ed52403c976"],["/tags/Windows/index.html","dd2b5a0ac3793c92a23f9171a57a1aa1"],["/tags/ZooKeeper/index.html","d92640c3a5b363808a762a0156ed2f21"],["/tags/bfs/index.html","0e79797b5ce0a56e589539a665b49723"],["/tags/dfs/index.html","60902b9b15949b643e1d3ce9fc6679d9"],["/tags/folium/index.html","f6de49ccbeedf1fc7e23e991f427a108"],["/tags/git/index.html","56feaac2ee59f6e97de02499033f1498"],["/tags/index.html","6b97298b564080464ecac28d22374708"],["/tags/latex/index.html","6aa9d813cb0404c2525f338c70efb923"],["/tags/中间件/index.html","796ffd3a37ed415fa2e8713a287343f3"],["/tags/二分查找/index.html","99ebb4a8c06390c7b06da20feff98362"],["/tags/优化类/index.html","81f96424cf41c30dc4d8edc8ad639022"],["/tags/前端/index.html","a31e094c5c4760ccdf7b495fa2f3ecb9"],["/tags/前缀和与差分/index.html","1ae0b4dc8d6c3bde6c493041cf977d40"],["/tags/动态规划/index.html","96fecfd892536e5c153f7c894b8dc5a9"],["/tags/动态规划/page/2/index.html","2863b5a42ca1b68a3cb4f16793bee015"],["/tags/博客搭建/index.html","64d9bed5a8b09e4533874de7e8bb565b"],["/tags/图论/index.html","1f5ed6ce7df22dd0a70998890a53ca48"],["/tags/大数据/index.html","9acfffe54543ab3ce3702df174eae23b"],["/tags/大数据/page/2/index.html","65360ef9c836161ad966457c0006b653"],["/tags/操作系统/index.html","afd10ba5174add1844ef536224dd70a6"],["/tags/数学建模/index.html","7e44c9ebce0ab250163723d5af50a798"],["/tags/数据库/index.html","8d0e9cf644c3b321d4782e7bd24cf235"],["/tags/数据结构和算法/index.html","0c1a559fb76520b32a19636e38972d00"],["/tags/数据结构和算法/page/2/index.html","b1cccbb6273fe872f32d16b5e2ea035b"],["/tags/数据结构和算法/page/3/index.html","86745e63cf154eaaf79146e477d1632c"],["/tags/数据结构和算法/page/4/index.html","9bd2e1ceae9a306d53766263b2673386"],["/tags/数组和字符串/index.html","0b30ec4b63464533a61f501d1413dbf5"],["/tags/数论/index.html","e5a6a465aedffba8b0fefb3d0042c7f1"],["/tags/枚举类/index.html","b9b523132b6984f24afcee5f0597954d"],["/tags/栈和队列/index.html","ba2f2af85d3e342674bb74c7cc72f9da"],["/tags/树论/index.html","3223f517048fb05ae1bbcf3a41e3bd68"],["/tags/测试/index.html","175feec01f82a5ed937921924f53d932"],["/tags/环境/index.html","1daaacdb1e9895c33ba72c2727e3fc52"],["/tags/环境变量/index.html","f4de4258633da534f9eb2e78a1e6e669"],["/tags/绘图/index.html","cf4ebd8aae9d66ee62ee7335fa6ea2f1"],["/tags/编程工具/index.html","cf266257a344219baf8169e4092bd1f1"],["/tags/编程环境/index.html","01e7e5d799216bf372e862aeb60d81a6"],["/tags/网络编程/index.html","c655bbb132460cc44afd743ce8e06f36"],["/tags/英语语法/index.html","5b230684475dcbbf43d92dcc3c3fd690"],["/tags/计算机操作系统/index.html","5598d2db50c95fc854bc4d0e0ff6e45f"],["/tags/论文/index.html","05811a1cfff80145eaa190b8a58895c7"],["/tags/资源下载/index.html","5b1eb436a64120e93565b7d90c7643a0"],["/tags/链表/index.html","a28c47c49c082d0e78d7d775a49de5ef"],["/tags/集合/index.html","7c1118e8945f65b88fc06e1bcfc52540"],["/tags/集群/index.html","891271c57c09dd1d9c627d41aeea1618"]];
var cacheName = 'sw-precache-v3--' + (self.registration ? self.registration.scope : '');
var firstRegister = 1; // 默认1是首次安装SW， 0是SW更新


var ignoreUrlParametersMatching = [/^utm_/];


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var cleanResponse = function (originalResponse) {
    // 如果没有重定向响应，不需干啥
    if (!originalResponse.redirected) {
        return Promise.resolve(originalResponse);
    }

    // Firefox 50 及以下不知处 Response.body 流, 所以我们需要读取整个body以blob形式返回。
    var bodyPromise = 'body' in originalResponse ?
        Promise.resolve(originalResponse.body) :
        originalResponse.blob();

    return bodyPromise.then(function (body) {
        // new Response() 可同时支持 stream or Blob.
        return new Response(body, {
            headers: originalResponse.headers,
            status: originalResponse.status,
            statusText: originalResponse.statusText
        });
    });
};

var createCacheKey = function (originalUrl, paramName, paramValue,
    dontCacheBustUrlsMatching) {

    // 创建一个新的URL对象，避免影响原始URL
    var url = new URL(originalUrl);

    // 如果 dontCacheBustUrlsMatching 值没有设置，或是没有匹配到，将值拼接到url.serach后
    if (!dontCacheBustUrlsMatching ||
        !(url.pathname.match(dontCacheBustUrlsMatching))) {
        url.search += (url.search ? '&' : '') +
            encodeURIComponent(paramName) + '=' + encodeURIComponent(paramValue);
    }

    return url.toString();
};

var isPathWhitelisted = function (whitelist, absoluteUrlString) {
    // 如果 whitelist 是空数组，则认为全部都在白名单内
    if (whitelist.length === 0) {
        return true;
    }

    // 否则逐个匹配正则匹配并返回
    var path = (new URL(absoluteUrlString)).pathname;
    return whitelist.some(function (whitelistedPathRegex) {
        return path.match(whitelistedPathRegex);
    });
};

var stripIgnoredUrlParameters = function (originalUrl,
    ignoreUrlParametersMatching) {
    var url = new URL(originalUrl);
    // 移除 hash; 查看 https://github.com/GoogleChrome/sw-precache/issues/290
    url.hash = '';

    url.search = url.search.slice(1) // 是否包含 '?'
        .split('&') // 分割成数组 'key=value' 的形式
        .map(function (kv) {
            return kv.split('='); // 分割每个 'key=value' 字符串成 [key, value] 形式
        })
        .filter(function (kv) {
            return ignoreUrlParametersMatching.every(function (ignoredRegex) {
                return !ignoredRegex.test(kv[0]); // 如果 key 没有匹配到任何忽略参数正则，就 Return true
            });
        })
        .map(function (kv) {
            return kv.join('='); // 重新把 [key, value] 格式转换为 'key=value' 字符串
        })
        .join('&'); // 将所有参数 'key=value' 以 '&' 拼接

    return url.toString();
};


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var hashParamName = '_sw-precache';
var urlsToCacheKeys = new Map(
    precacheConfig.map(function (item) {
        var relativeUrl = item[0];
        var hash = item[1];
        var absoluteUrl = new URL(relativeUrl, self.location);
        var cacheKey = createCacheKey(absoluteUrl, hashParamName, hash, false);
        return [absoluteUrl.toString(), cacheKey];
    })
);

function setOfCachedUrls(cache) {
    return cache.keys().then(function (requests) {
        // 如果原cacheName中没有缓存任何收，就默认是首次安装，否则认为是SW更新
        if (requests && requests.length > 0) {
            firstRegister = 0; // SW更新
        }
        return requests.map(function (request) {
            return request.url;
        });
    }).then(function (urls) {
        return new Set(urls);
    });
}

self.addEventListener('install', function (event) {
    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return setOfCachedUrls(cache).then(function (cachedUrls) {
                return Promise.all(
                    Array.from(urlsToCacheKeys.values()).map(function (cacheKey) {
                        // 如果缓存中没有匹配到cacheKey，添加进去
                        if (!cachedUrls.has(cacheKey)) {
                            var request = new Request(cacheKey, { credentials: 'same-origin' });
                            return fetch(request).then(function (response) {
                                // 只要返回200才能继续，否则直接抛错
                                if (!response.ok) {
                                    throw new Error('Request for ' + cacheKey + ' returned a ' +
                                        'response with status ' + response.status);
                                }

                                return cleanResponse(response).then(function (responseToCache) {
                                    return cache.put(cacheKey, responseToCache);
                                });
                            });
                        }
                    })
                );
            });
        })
            .then(function () {
            
            // 强制 SW 状态 installing -> activate
            return self.skipWaiting();
            
        })
    );
});

self.addEventListener('activate', function (event) {
    var setOfExpectedUrls = new Set(urlsToCacheKeys.values());

    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return cache.keys().then(function (existingRequests) {
                return Promise.all(
                    existingRequests.map(function (existingRequest) {
                        // 删除原缓存中相同键值内容
                        if (!setOfExpectedUrls.has(existingRequest.url)) {
                            return cache.delete(existingRequest);
                        }
                    })
                );
            });
        }).then(function () {
            
            return self.clients.claim();
            
        }).then(function () {
                // 如果是首次安装 SW 时, 不发送更新消息（是否是首次安装，通过指定cacheName 中是否有缓存信息判断）
                // 如果不是首次安装，则是内容有更新，需要通知页面重载更新
                if (!firstRegister) {
                    return self.clients.matchAll()
                        .then(function (clients) {
                            if (clients && clients.length) {
                                clients.forEach(function (client) {
                                    client.postMessage('sw.update');
                                })
                            }
                        })
                }
            })
    );
});



    self.addEventListener('fetch', function (event) {
        if (event.request.method === 'GET') {

            // 是否应该 event.respondWith()，需要我们逐步的判断
            // 而且也方便了后期做特殊的特殊
            var shouldRespond;


            // 首先去除已配置的忽略参数及hash
            // 查看缓存简直中是否包含该请求，包含就将shouldRespond 设为true
            var url = stripIgnoredUrlParameters(event.request.url, ignoreUrlParametersMatching);
            shouldRespond = urlsToCacheKeys.has(url);

            // 如果 shouldRespond 是 false, 我们在url后默认增加 'index.html'
            // (或者是你在配置文件中自行配置的 directoryIndex 参数值)，继续查找缓存列表
            var directoryIndex = 'index.html';
            if (!shouldRespond && directoryIndex) {
                url = addDirectoryIndex(url, directoryIndex);
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 仍是 false，检查是否是navigation
            // request， 如果是的话，判断是否能与 navigateFallbackWhitelist 正则列表匹配
            var navigateFallback = '';
            if (!shouldRespond &&
                navigateFallback &&
                (event.request.mode === 'navigate') &&
                isPathWhitelisted([], event.request.url)
            ) {
                url = new URL(navigateFallback, self.location).toString();
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 被置为 true
            // 则 event.respondWith()匹配缓存返回结果，匹配不成就直接请求.
            if (shouldRespond) {
                event.respondWith(
                    caches.open(cacheName).then(function (cache) {
                        return cache.match(urlsToCacheKeys.get(url)).then(function (response) {
                            if (response) {
                                return response;
                            }
                            throw Error('The cached response that was expected is missing.');
                        });
                    }).catch(function (e) {
                        // 如果捕获到异常错误，直接返回 fetch() 请求资源
                        console.warn('Couldn\'t serve response for "%s" from cache: %O', event.request.url, e);
                        return fetch(event.request);
                    })
                );
            }
        }
    });



// *** Start of auto-included sw-toolbox code. ***
/* 
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var t;t="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,t.toolbox=e()}}(function(){return function e(t,n,r){function o(c,s){if(!n[c]){if(!t[c]){var a="function"==typeof require&&require;if(!s&&a)return a(c,!0);if(i)return i(c,!0);var u=new Error("Cannot find module '"+c+"'");throw u.code="MODULE_NOT_FOUND",u}var f=n[c]={exports:{}};t[c][0].call(f.exports,function(e){var n=t[c][1][e];return o(n?n:e)},f,f.exports,e,t,n,r)}return n[c].exports}for(var i="function"==typeof require&&require,c=0;c<r.length;c++)o(r[c]);return o}({1:[function(e,t,n){"use strict";function r(e,t){t=t||{};var n=t.debug||m.debug;n&&console.log("[sw-toolbox] "+e)}function o(e){var t;return e&&e.cache&&(t=e.cache.name),t=t||m.cache.name,caches.open(t)}function i(e,t){t=t||{};var n=t.successResponses||m.successResponses;return fetch(e.clone()).then(function(r){return"GET"===e.method&&n.test(r.status)&&o(t).then(function(n){n.put(e,r).then(function(){var r=t.cache||m.cache;(r.maxEntries||r.maxAgeSeconds)&&r.name&&c(e,n,r)})}),r.clone()})}function c(e,t,n){var r=s.bind(null,e,t,n);d=d?d.then(r):r()}function s(e,t,n){var o=e.url,i=n.maxAgeSeconds,c=n.maxEntries,s=n.name,a=Date.now();return r("Updating LRU order for "+o+". Max entries is "+c+", max age is "+i),g.getDb(s).then(function(e){return g.setTimestampForUrl(e,o,a)}).then(function(e){return g.expireEntries(e,c,i,a)}).then(function(e){r("Successfully updated IDB.");var n=e.map(function(e){return t.delete(e)});return Promise.all(n).then(function(){r("Done with cache cleanup.")})}).catch(function(e){r(e)})}function a(e,t,n){return r("Renaming cache: ["+e+"] to ["+t+"]",n),caches.delete(t).then(function(){return Promise.all([caches.open(e),caches.open(t)]).then(function(t){var n=t[0],r=t[1];return n.keys().then(function(e){return Promise.all(e.map(function(e){return n.match(e).then(function(t){return r.put(e,t)})}))}).then(function(){return caches.delete(e)})})})}function u(e,t){return o(t).then(function(t){return t.add(e)})}function f(e,t){return o(t).then(function(t){return t.delete(e)})}function h(e){e instanceof Promise||p(e),m.preCacheItems=m.preCacheItems.concat(e)}function p(e){var t=Array.isArray(e);if(t&&e.forEach(function(e){"string"==typeof e||e instanceof Request||(t=!1)}),!t)throw new TypeError("The precache method expects either an array of strings and/or Requests or a Promise that resolves to an array of strings and/or Requests.");return e}function l(e,t,n){if(!e)return!1;if(t){var r=e.headers.get("date");if(r){var o=new Date(r);if(o.getTime()+1e3*t<n)return!1}}return!0}var d,m=e("./options"),g=e("./idb-cache-expiration");t.exports={debug:r,fetchAndCache:i,openCache:o,renameCache:a,cache:u,uncache:f,precache:h,validatePrecacheInput:p,isResponseFresh:l}},{"./idb-cache-expiration":2,"./options":4}],2:[function(e,t,n){"use strict";function r(e){return new Promise(function(t,n){var r=indexedDB.open(u+e,f);r.onupgradeneeded=function(){var e=r.result.createObjectStore(h,{keyPath:p});e.createIndex(l,l,{unique:!1})},r.onsuccess=function(){t(r.result)},r.onerror=function(){n(r.error)}})}function o(e){return e in d||(d[e]=r(e)),d[e]}function i(e,t,n){return new Promise(function(r,o){var i=e.transaction(h,"readwrite"),c=i.objectStore(h);c.put({url:t,timestamp:n}),i.oncomplete=function(){r(e)},i.onabort=function(){o(i.error)}})}function c(e,t,n){return t?new Promise(function(r,o){var i=1e3*t,c=[],s=e.transaction(h,"readwrite"),a=s.objectStore(h),u=a.index(l);u.openCursor().onsuccess=function(e){var t=e.target.result;if(t&&n-i>t.value[l]){var r=t.value[p];c.push(r),a.delete(r),t.continue()}},s.oncomplete=function(){r(c)},s.onabort=o}):Promise.resolve([])}function s(e,t){return t?new Promise(function(n,r){var o=[],i=e.transaction(h,"readwrite"),c=i.objectStore(h),s=c.index(l),a=s.count();s.count().onsuccess=function(){var e=a.result;e>t&&(s.openCursor().onsuccess=function(n){var r=n.target.result;if(r){var i=r.value[p];o.push(i),c.delete(i),e-o.length>t&&r.continue()}})},i.oncomplete=function(){n(o)},i.onabort=r}):Promise.resolve([])}function a(e,t,n,r){return c(e,n,r).then(function(n){return s(e,t).then(function(e){return n.concat(e)})})}var u="sw-toolbox-",f=1,h="store",p="url",l="timestamp",d={};t.exports={getDb:o,setTimestampForUrl:i,expireEntries:a}},{}],3:[function(e,t,n){"use strict";function r(e){var t=a.match(e.request);t?e.respondWith(t(e.request)):a.default&&"GET"===e.request.method&&0===e.request.url.indexOf("http")&&e.respondWith(a.default(e.request))}function o(e){s.debug("activate event fired");var t=u.cache.name+"$$$inactive$$$";e.waitUntil(s.renameCache(t,u.cache.name))}function i(e){return e.reduce(function(e,t){return e.concat(t)},[])}function c(e){var t=u.cache.name+"$$$inactive$$$";s.debug("install event fired"),s.debug("creating cache ["+t+"]"),e.waitUntil(s.openCache({cache:{name:t}}).then(function(e){return Promise.all(u.preCacheItems).then(i).then(s.validatePrecacheInput).then(function(t){return s.debug("preCache list: "+(t.join(", ")||"(none)")),e.addAll(t)})}))}e("serviceworker-cache-polyfill");var s=e("./helpers"),a=e("./router"),u=e("./options");t.exports={fetchListener:r,activateListener:o,installListener:c}},{"./helpers":1,"./options":4,"./router":6,"serviceworker-cache-polyfill":16}],4:[function(e,t,n){"use strict";var r;r=self.registration?self.registration.scope:self.scope||new URL("./",self.location).href,t.exports={cache:{name:"$$$toolbox-cache$$$"+r+"$$$",maxAgeSeconds:null,maxEntries:null},debug:!1,networkTimeoutSeconds:null,preCacheItems:[],successResponses:/^0|([123]\d\d)|(40[14567])|410$/}},{}],5:[function(e,t,n){"use strict";var r=new URL("./",self.location),o=r.pathname,i=e("path-to-regexp"),c=function(e,t,n,r){t instanceof RegExp?this.fullUrlRegExp=t:(0!==t.indexOf("/")&&(t=o+t),this.keys=[],this.regexp=i(t,this.keys)),this.method=e,this.options=r,this.handler=n};c.prototype.makeHandler=function(e){var t;if(this.regexp){var n=this.regexp.exec(e);t={},this.keys.forEach(function(e,r){t[e.name]=n[r+1]})}return function(e){return this.handler(e,t,this.options)}.bind(this)},t.exports=c},{"path-to-regexp":15}],6:[function(e,t,n){"use strict";function r(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var o=e("./route"),i=e("./helpers"),c=function(e,t){for(var n=e.entries(),r=n.next(),o=[];!r.done;){var i=new RegExp(r.value[0]);i.test(t)&&o.push(r.value[1]),r=n.next()}return o},s=function(){this.routes=new Map,this.routes.set(RegExp,new Map),this.default=null};["get","post","put","delete","head","any"].forEach(function(e){s.prototype[e]=function(t,n,r){return this.add(e,t,n,r)}}),s.prototype.add=function(e,t,n,c){c=c||{};var s;t instanceof RegExp?s=RegExp:(s=c.origin||self.location.origin,s=s instanceof RegExp?s.source:r(s)),e=e.toLowerCase();var a=new o(e,t,n,c);this.routes.has(s)||this.routes.set(s,new Map);var u=this.routes.get(s);u.has(e)||u.set(e,new Map);var f=u.get(e),h=a.regexp||a.fullUrlRegExp;f.has(h.source)&&i.debug('"'+t+'" resolves to same regex as existing route.'),f.set(h.source,a)},s.prototype.matchMethod=function(e,t){var n=new URL(t),r=n.origin,o=n.pathname;return this._match(e,c(this.routes,r),o)||this._match(e,[this.routes.get(RegExp)],t)},s.prototype._match=function(e,t,n){if(0===t.length)return null;for(var r=0;r<t.length;r++){var o=t[r],i=o&&o.get(e.toLowerCase());if(i){var s=c(i,n);if(s.length>0)return s[0].makeHandler(n)}}return null},s.prototype.match=function(e){return this.matchMethod(e.method,e.url)||this.matchMethod("any",e.url)},t.exports=new s},{"./helpers":1,"./route":5}],7:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache first ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(t){var r=n.cache||o.cache,c=Date.now();return i.isResponseFresh(t,r.maxAgeSeconds,c)?t:i.fetchAndCache(e,n)})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],8:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache only ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(e){var t=n.cache||o.cache,r=Date.now();if(i.isResponseFresh(e,t.maxAgeSeconds,r))return e})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],9:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: fastest ["+e.url+"]",n),new Promise(function(r,c){var s=!1,a=[],u=function(e){a.push(e.toString()),s?c(new Error('Both cache and network failed: "'+a.join('", "')+'"')):s=!0},f=function(e){e instanceof Response?r(e):u("No result returned")};o.fetchAndCache(e.clone(),n).then(f,u),i(e,t,n).then(f,u)})}var o=e("../helpers"),i=e("./cacheOnly");t.exports=r},{"../helpers":1,"./cacheOnly":8}],10:[function(e,t,n){t.exports={networkOnly:e("./networkOnly"),networkFirst:e("./networkFirst"),cacheOnly:e("./cacheOnly"),cacheFirst:e("./cacheFirst"),fastest:e("./fastest")}},{"./cacheFirst":7,"./cacheOnly":8,"./fastest":9,"./networkFirst":11,"./networkOnly":12}],11:[function(e,t,n){"use strict";function r(e,t,n){n=n||{};var r=n.successResponses||o.successResponses,c=n.networkTimeoutSeconds||o.networkTimeoutSeconds;return i.debug("Strategy: network first ["+e.url+"]",n),i.openCache(n).then(function(t){var s,a,u=[];if(c){var f=new Promise(function(r){s=setTimeout(function(){t.match(e).then(function(e){var t=n.cache||o.cache,c=Date.now(),s=t.maxAgeSeconds;i.isResponseFresh(e,s,c)&&r(e)})},1e3*c)});u.push(f)}var h=i.fetchAndCache(e,n).then(function(e){if(s&&clearTimeout(s),r.test(e.status))return e;throw i.debug("Response was an HTTP error: "+e.statusText,n),a=e,new Error("Bad response")}).catch(function(r){return i.debug("Network or response error, fallback to cache ["+e.url+"]",n),t.match(e).then(function(e){if(e)return e;if(a)return a;throw r})});return u.push(h),Promise.race(u)})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],12:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: network only ["+e.url+"]",n),fetch(e)}var o=e("../helpers");t.exports=r},{"../helpers":1}],13:[function(e,t,n){"use strict";var r=e("./options"),o=e("./router"),i=e("./helpers"),c=e("./strategies"),s=e("./listeners");i.debug("Service Worker Toolbox is loading"),self.addEventListener("install",s.installListener),self.addEventListener("activate",s.activateListener),self.addEventListener("fetch",s.fetchListener),t.exports={networkOnly:c.networkOnly,networkFirst:c.networkFirst,cacheOnly:c.cacheOnly,cacheFirst:c.cacheFirst,fastest:c.fastest,router:o,options:r,cache:i.cache,uncache:i.uncache,precache:i.precache}},{"./helpers":1,"./listeners":3,"./options":4,"./router":6,"./strategies":10}],14:[function(e,t,n){t.exports=Array.isArray||function(e){return"[object Array]"==Object.prototype.toString.call(e)}},{}],15:[function(e,t,n){function r(e,t){for(var n,r=[],o=0,i=0,c="",s=t&&t.delimiter||"/";null!=(n=x.exec(e));){var f=n[0],h=n[1],p=n.index;if(c+=e.slice(i,p),i=p+f.length,h)c+=h[1];else{var l=e[i],d=n[2],m=n[3],g=n[4],v=n[5],w=n[6],y=n[7];c&&(r.push(c),c="");var b=null!=d&&null!=l&&l!==d,E="+"===w||"*"===w,R="?"===w||"*"===w,k=n[2]||s,$=g||v;r.push({name:m||o++,prefix:d||"",delimiter:k,optional:R,repeat:E,partial:b,asterisk:!!y,pattern:$?u($):y?".*":"[^"+a(k)+"]+?"})}}return i<e.length&&(c+=e.substr(i)),c&&r.push(c),r}function o(e,t){return s(r(e,t))}function i(e){return encodeURI(e).replace(/[\/?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function c(e){return encodeURI(e).replace(/[?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function s(e){for(var t=new Array(e.length),n=0;n<e.length;n++)"object"==typeof e[n]&&(t[n]=new RegExp("^(?:"+e[n].pattern+")$"));return function(n,r){for(var o="",s=n||{},a=r||{},u=a.pretty?i:encodeURIComponent,f=0;f<e.length;f++){var h=e[f];if("string"!=typeof h){var p,l=s[h.name];if(null==l){if(h.optional){h.partial&&(o+=h.prefix);continue}throw new TypeError('Expected "'+h.name+'" to be defined')}if(v(l)){if(!h.repeat)throw new TypeError('Expected "'+h.name+'" to not repeat, but received `'+JSON.stringify(l)+"`");if(0===l.length){if(h.optional)continue;throw new TypeError('Expected "'+h.name+'" to not be empty')}for(var d=0;d<l.length;d++){if(p=u(l[d]),!t[f].test(p))throw new TypeError('Expected all "'+h.name+'" to match "'+h.pattern+'", but received `'+JSON.stringify(p)+"`");o+=(0===d?h.prefix:h.delimiter)+p}}else{if(p=h.asterisk?c(l):u(l),!t[f].test(p))throw new TypeError('Expected "'+h.name+'" to match "'+h.pattern+'", but received "'+p+'"');o+=h.prefix+p}}else o+=h}return o}}function a(e){return e.replace(/([.+*?=^!:${}()[\]|\/\\])/g,"\\$1")}function u(e){return e.replace(/([=!:$\/()])/g,"\\$1")}function f(e,t){return e.keys=t,e}function h(e){return e.sensitive?"":"i"}function p(e,t){var n=e.source.match(/\((?!\?)/g);if(n)for(var r=0;r<n.length;r++)t.push({name:r,prefix:null,delimiter:null,optional:!1,repeat:!1,partial:!1,asterisk:!1,pattern:null});return f(e,t)}function l(e,t,n){for(var r=[],o=0;o<e.length;o++)r.push(g(e[o],t,n).source);var i=new RegExp("(?:"+r.join("|")+")",h(n));return f(i,t)}function d(e,t,n){return m(r(e,n),t,n)}function m(e,t,n){v(t)||(n=t||n,t=[]),n=n||{};for(var r=n.strict,o=n.end!==!1,i="",c=0;c<e.length;c++){var s=e[c];if("string"==typeof s)i+=a(s);else{var u=a(s.prefix),p="(?:"+s.pattern+")";t.push(s),s.repeat&&(p+="(?:"+u+p+")*"),p=s.optional?s.partial?u+"("+p+")?":"(?:"+u+"("+p+"))?":u+"("+p+")",i+=p}}var l=a(n.delimiter||"/"),d=i.slice(-l.length)===l;return r||(i=(d?i.slice(0,-l.length):i)+"(?:"+l+"(?=$))?"),i+=o?"$":r&&d?"":"(?="+l+"|$)",f(new RegExp("^"+i,h(n)),t)}function g(e,t,n){return v(t)||(n=t||n,t=[]),n=n||{},e instanceof RegExp?p(e,t):v(e)?l(e,t,n):d(e,t,n)}var v=e("isarray");t.exports=g,t.exports.parse=r,t.exports.compile=o,t.exports.tokensToFunction=s,t.exports.tokensToRegExp=m;var x=new RegExp(["(\\\\.)","([\\/.])?(?:(?:\\:(\\w+)(?:\\(((?:\\\\.|[^\\\\()])+)\\))?|\\(((?:\\\\.|[^\\\\()])+)\\))([+*?])?|(\\*))"].join("|"),"g")},{isarray:14}],16:[function(e,t,n){!function(){var e=Cache.prototype.addAll,t=navigator.userAgent.match(/(Firefox|Chrome)\/(\d+\.)/);if(t)var n=t[1],r=parseInt(t[2]);e&&(!t||"Firefox"===n&&r>=46||"Chrome"===n&&r>=50)||(Cache.prototype.addAll=function(e){function t(e){this.name="NetworkError",this.code=19,this.message=e}var n=this;return t.prototype=Object.create(Error.prototype),Promise.resolve().then(function(){if(arguments.length<1)throw new TypeError;return e=e.map(function(e){return e instanceof Request?e:String(e)}),Promise.all(e.map(function(e){"string"==typeof e&&(e=new Request(e));var n=new URL(e.url).protocol;if("http:"!==n&&"https:"!==n)throw new t("Invalid scheme");return fetch(e.clone())}))}).then(function(r){if(r.some(function(e){return!e.ok}))throw new t("Incorrect response status");return Promise.all(r.map(function(t,r){return n.put(e[r],t)}))}).then(function(){})},Cache.prototype.add=function(e){return this.addAll([e])})}()},{}]},{},[13])(13)});


// *** End of auto-included sw-toolbox code. ***



// Runtime cache 配置转换后的 toolbox 代码.

toolbox.router.get("/*", toolbox.cacheFirst, {"origin":"unpkg.com"});
toolbox.router.get("/npm/*", toolbox.cacheFirst, {"origin":"cdn.jsdelivr.net"});





/* eslint-enable */
