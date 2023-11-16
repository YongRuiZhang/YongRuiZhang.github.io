/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e5a5c7e7d6c698a6815d9fd58b87c463"],["/about/index.html","e02a342176586644851cd7c220ce0f1c"],["/archives/2023/01/index.html","a52df9117a8e6d46b148810186b87c56"],["/archives/2023/02/index.html","c5140c5756f1c7ea7380decac5a42e6d"],["/archives/2023/02/page/2/index.html","15ba655f6e17e28977f90cdc520241c2"],["/archives/2023/03/index.html","0956a8b6b97cc6e0ee3d9090e5b5595a"],["/archives/2023/05/index.html","834031719171f5544aeca45533f4e0b9"],["/archives/2023/06/index.html","e10b8019d7dfd33bfb52b35ede5e8d5a"],["/archives/2023/09/index.html","0b1f99b547d640cd3141c0945d6ada53"],["/archives/2023/index.html","1cd524629cf767f3b19bb22738fa4d26"],["/archives/2023/page/2/index.html","b7c1689321329f557d54bd23545e7d96"],["/archives/2023/page/3/index.html","e043b32f944025e7b71003ac1c2bf22b"],["/archives/2023/page/4/index.html","66711bc110788c406de345b23f9e43a9"],["/archives/index.html","6716199c94adbca9c41b5fbbf35ea60d"],["/archives/page/2/index.html","8cfdf998d5cde91ceee71073ccaa8fad"],["/archives/page/3/index.html","4606a9c4ae17e7410cbf9c9b117e88be"],["/archives/page/4/index.html","fbb010a11473f35cba384dd4adc0a012"],["/baidu_verify_codeva-qQP2iZOMLX.html","72bc0455bc01f128e2e41a048e5f3f95"],["/categories/Java/index.html","408b8d3f9e914b604ba62371f51551b7"],["/categories/Java/后端/index.html","331d295f59a93260ecdaf6094991f8eb"],["/categories/Java/基础/index.html","217289381422e848dd04e121ef87c59b"],["/categories/Java/基础/集合/index.html","31e332cba02b8b35af302bd06e67cab3"],["/categories/Python/index.html","5203bd0fa1a56e2a7e9760d9d0bfe4fe"],["/categories/Python/编程环境/index.html","23ad31c8d58b08bf755ea6853e1c3aaa"],["/categories/R语言/index.html","6caddf664b2ec0551d640627dc1cb8c8"],["/categories/R语言/编程环境/index.html","86a7cd064d1858959e3a70ffa0788270"],["/categories/index.html","834197390a06208c4c57b1894820aa2a"],["/categories/中间件/index.html","d5a0a3e30e6020624862da61fe6def16"],["/categories/前端/Vue/index.html","213905e1d77f319f2c23c6acd5f706d1"],["/categories/前端/index.html","68c1923ee9c9d6f855771c05bc1ee8d1"],["/categories/大数据开发/ElasticSearch/index.html","fc2c11b234cb10d9a0792254b36f7ce9"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","dc1b36d7844e28a60d9da93ac423d194"],["/categories/大数据开发/HBase/index.html","f11a6eebf296a586b99ab58d56e6e3c3"],["/categories/大数据开发/HBase/学习笔记/index.html","fb21e7f3fffd6d268dc7484a3862a034"],["/categories/大数据开发/HBase/环境搭建/index.html","8ed32057c198c1393a0a503beebe15c5"],["/categories/大数据开发/Hadoop/index.html","8b2914a0e85646d6bc45b98abf610559"],["/categories/大数据开发/Hadoop/技术/index.html","29d3ae55f76dd68accf399ebe5357556"],["/categories/大数据开发/Hadoop/环境搭建/index.html","eb94f74897068b2d70b8f51eeb5d5420"],["/categories/大数据开发/Redis/index.html","b744547d5d5e1df89be10f01d8d37d0c"],["/categories/大数据开发/Redis/技术/index.html","f06448fda3bdc94bd52221dfe9878bbf"],["/categories/大数据开发/Redis/环境搭建/index.html","fc109ea9469599e7258ec9e0fa72d562"],["/categories/大数据开发/Spark/index.html","0a9eb6463e1a58e8ee531d1f94a32ac6"],["/categories/大数据开发/Spark/环境搭建/index.html","1739be503dc6345acb08b1ba76f63198"],["/categories/大数据开发/Zookeeper/index.html","ae6224083bcbdbf2b21fa10d10202db1"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9a62bb8486eb7715c0c468a2803ee442"],["/categories/大数据开发/index.html","1961d16a48cec1149f861a44c830c8c7"],["/categories/操作系统/Linux/index.html","67a03ed38c85e6655dacada3774faaad"],["/categories/操作系统/Mac/index.html","4ff8d5a3f8e406c7950d8bdb56b0ac95"],["/categories/操作系统/Windows/index.html","5206a33ee166db692f3922dbaa77034e"],["/categories/操作系统/index.html","3aaf6bc0491803d24982f5cb18e2a91e"],["/categories/数学建模/index.html","f149a6bab2507272b87f58fd7ff974a5"],["/categories/数学建模/latex/index.html","abcee05c86123a964f28fa669e2e3e32"],["/categories/数学建模/优化类/index.html","cbc9d8f270ac57b98e24467c44539d8f"],["/categories/数学建模/优化类/现代优化算法/index.html","1bebef7e0a553778868fbf7d889c2d06"],["/categories/数学建模/优化类/规划类/index.html","121d170f7928bc7cb9a0df2634a13913"],["/categories/数学建模/绘图/index.html","71a5829a1092cf7389e0bd36c52e5693"],["/categories/数据库/MySQL/index.html","5279f361f4feabc992b21bb889917289"],["/categories/数据库/index.html","e895eb4baf16661a9e4d1ef2a846ffa5"],["/categories/数据结构和算法/index.html","2daa26dcf28bd7f9e7fb1173a965dd43"],["/categories/数据结构和算法/page/2/index.html","3069d7f1e3cf29bad25a87fbce1fd994"],["/categories/数据结构和算法/基本原理/bfs/index.html","96da773e6ca5301c6d9d794a0edc0d11"],["/categories/数据结构和算法/基本原理/dfs/index.html","804c71452ae0a5c10dc54ed7d06d63b5"],["/categories/数据结构和算法/基本原理/index.html","71397d9106a89ae1cf1d16eed265a73c"],["/categories/数据结构和算法/基本原理/动态规划/index.html","717dcc1f5d85b1039d9bf7aef5f072e4"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0ca451b71e12456cfb3ca227f150d99c"],["/categories/数据结构和算法/基本原理/图论/index.html","f43d0fefd58dac09ee0732c8702ec297"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1a98eda0a81f8dbac66c2d74bea84233"],["/categories/数据结构和算法/基本原理/数论/index.html","6568ab18c5eaba2b914520db5d277775"],["/categories/数据结构和算法/基本原理/树论/index.html","022dd1552a7d6251adcfae34bac5c84e"],["/categories/数据结构和算法/基本原理/链表/index.html","649b33034778777a65d81124b3a9f9b5"],["/categories/数据结构和算法/算法题/index.html","aeaaf7759aa8002dc68d97eaf6168fcf"],["/categories/数据结构和算法/算法题/二分查找/index.html","324ac5daf4c7f6c81d5bcfbdeba074ba"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","721e4471a9592042bf74e593637aabac"],["/categories/数据结构和算法/算法题/动态规划/index.html","906bc37ae79b92b28c6ec20505003ee8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","63b2b7b1a3e98ebc275a45e8354597d1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","fbc5cee7bd6714ce58d5ce148daeb991"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5e67f8e5084ce6c8a8cfc6e7653e5252"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4aaabb44e673205d3aba6a6cbc5a843f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5d4650fc28b21684be5f38f8235b7338"],["/categories/数据结构和算法/算法题/树论/index.html","d0e87c547baa5208fb8d2acfa7e300e2"],["/categories/杂七杂八/index.html","dcb3cc680bb31d5f6d7bd8963f97ed1c"],["/categories/杂七杂八/博客搭建/index.html","2e97ae2b777abbd9e3a91592f600a3af"],["/categories/编程工具下载/index.html","99aa60ff4afc34d2de1ec776902d2375"],["/categories/编程环境/index.html","ae53b9cd15d8d06fbde75d975eb180e5"],["/categories/编程环境/大数据/index.html","415ef1e786394b10cad1eb5c8d082c53"],["/categories/英语学习/index.html","42f189406ca6f3256cc2323bcd3c80a4"],["/categories/英语学习/英语语法/index.html","6b1708f1f6d5acd53a53eba02174a42d"],["/comments/index.html","76efe10946a827d0d7007e05318971cf"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0ba186db7b6363cbc0d3e6b93c964918"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","27166069ce5c560a9d0096808cc79461"],["/movies/index.html","11a3b9d3c8d87bb3b1800f28160e517c"],["/music/index.html","75e0cd2f4d9c1f214a94a8beef921323"],["/page/2/index.html","867c79219e9c94f1c98b1e5a6b15a62e"],["/page/3/index.html","27f848837264d4e49263ae8b32fac539"],["/page/4/index.html","9e357e7a45f45efdf976ea48b7a267d2"],["/page/5/index.html","d5099f25c464ae12976ce6d8769d13eb"],["/page/6/index.html","040af954d3274eaa245985e5ba822542"],["/posts/1021360842.html","12c2c260aa572bc199b8fb3ca722e8bc"],["/posts/1120620192.html","06151e791360c514c52c2b06b28ffcdd"],["/posts/1141628095.html","592f3620d2ea21e759b855abebb24308"],["/posts/1168613674.html","b1b481d0f5566cf77e11de6c851d73f6"],["/posts/1219920510.html","3fd1b4616929ef07c192db1c9736a774"],["/posts/1222166338.html","cd3d780a7d4cc26df62c12665039d235"],["/posts/1259097482.html","0aed35ed1bd139817d1145692c89bf94"],["/posts/1271036369.html","ced746b90a7cb315862f2e7402feb65e"],["/posts/1312847445.html","fca2a97ec7cc88d23aa981758add28a8"],["/posts/135355774.html","502c763220cb77a4d72246fc357a37b7"],["/posts/1375344716.html","03306173b39976881e7128c405c7d552"],["/posts/1388991698.html","a99433b122a9652064f0f46c917becea"],["/posts/1410315814.html","91c1d7302031ca9ba8ebf4e21fbdd0f5"],["/posts/1452790229.html","6b8d55bd5238371c7643cb69c7ded60c"],["/posts/1470079884.html","0525ac26284b51cf4a2025668f6a5a76"],["/posts/1470079885.html","b21b4bdae6d6fedf86dc3cc460b8806a"],["/posts/1470079886.html","ec4010121ac13ccff288ebf1440c188f"],["/posts/1470079887.html","3649959523149d8fa57de1382a633ba1"],["/posts/1498536549.html","98812a636d4c09eb793664d528ca9385"],["/posts/1547067935.html","1e771a780ef1b43e4591101b0c28c4ca"],["/posts/1557866301.html","b3ad95deee32ac8b97e2edfec1653aeb"],["/posts/1571776361.html","1fec70970fa9badd95212a2d364550aa"],["/posts/1605124548.html","0660cced2739c84d98a4cf124a1283e8"],["/posts/1633036852.html","cf594b9df10858a0eb9446de05080ab6"],["/posts/1674202625.html","fab959323af73e72c87f8357ca9bf031"],["/posts/1765123828.html","cc6c44d754f961a5f54db3b9310d3bd2"],["/posts/1767336200.html","46fd484a8558c93687777d52b251aa6c"],["/posts/1776114197.html","4c57bc3f4e4a6cf0c5036f24714102f3"],["/posts/1817748743.html","2a2488f7a0b6c5e93f1191178c500e01"],["/posts/1925125395.html","a6ba8535256e0f814b289adb405edfa7"],["/posts/1966191251.html","72ee715d696683e22c5600a719a1448f"],["/posts/1987617322.html","19a1500de9244c52ed252e8000635711"],["/posts/1999788039.html","591a9ae678d57b45c2b07264d7a381cf"],["/posts/2075104059.html","2665804f3ec2b1b37b9dafadb9944053"],["/posts/2087796737.html","0a1b985de78aef0f6f8e89102b7e5eba"],["/posts/2106547339.html","062ee51d5a63b84a61113df6fbf7cda6"],["/posts/2207806286.html","3ceeff5d543ca4799ba5819100669e2f"],["/posts/2225903441.html","1e650359b5de578ed098681bdea48c11"],["/posts/2265610284.html","e91332fb474098eea62918a4e3c6c420"],["/posts/2281352001.html","af1cf17a7720ad53ea5d5e0276473407"],["/posts/2364755265.html","5fba18b28f7db75b76615e77a75190f7"],["/posts/2414116852.html","4fe00f24f5caad03f6e986e6cc857fab"],["/posts/2421785022.html","65ee113eb15f305f9b61b1338517bd7d"],["/posts/2482902029.html","7c808c8c19dd67077fd88e5daa12a7e3"],["/posts/2495386210.html","c6fdd72150233ead4557180e1b85f9be"],["/posts/2516528882.html","9e0f4d1066d5ea5d4d0a0f36f349548f"],["/posts/2526659543.html","76025e165fa2b0f6f773038e054a9a81"],["/posts/2529807823.html","34f535b985a63d6a07df6be16e64197a"],["/posts/2596601004.html","2b9903239b7f407c7cb52af153bb0898"],["/posts/2742438348.html","7fb49f60d14d53c471b8ce557d20fb3d"],["/posts/2888309600.html","eb7c6a2e0bfa2e0f423cf96397fa7e68"],["/posts/2891591958.html","75d7ef91dad881972d69dbd8eb062d92"],["/posts/2909934084.html","7ed793d037d315d722517206a2dd3a38"],["/posts/2920256992.html","ad6369241fc65e9baee902edcd140417"],["/posts/3005926051.html","510cff923c07498b0d66a726f965dcf9"],["/posts/309775400.html","3623cb910898bd5b08ce42974e213e80"],["/posts/3156194925.html","e23fa71111ceae715d3d79e73f916e2d"],["/posts/3169224211.html","0731658cba18770c5b3571bd12b21e5b"],["/posts/3213899550.html","f185dbabf31824797ff24df734d64202"],["/posts/3259212833.html","4f6a11e87981641e73e3c06858ec6a69"],["/posts/3266130344.html","d96f7b2635b6545353477a22dc862bac"],["/posts/3292663995.html","340af0b6e97a2f1fa8d4129378243746"],["/posts/3297135020.html","d6be793fbe248e5c3619cf23bbda6976"],["/posts/3306641566.html","ea8d07ed70ae732917e645996e664bc4"],["/posts/3312011324.html","21be681fcf7765167d158900940fe8cc"],["/posts/336911618.html","f5dda08bcf3437fee4063e7c2d819381"],["/posts/3402121571.html","1bee462b9d5f23b21ff88de9d9b37cf4"],["/posts/3405577485.html","751886e93c696a7d8ca6ae482cccc3c7"],["/posts/3498516849.html","b6350b47f39bd0bbad52ec7a01dc0820"],["/posts/3513711414.html","5a4095b0a057ea8c1913b494a63ef34d"],["/posts/3546711884.html","ac1424bde53f13c87393715a141980cd"],["/posts/3731385230.html","ff9c8ded2883fa1664208a5fc636cc56"],["/posts/3772089482.html","5dbca3a0293ae04e81b294fffe11f37d"],["/posts/386609427.html","67bf44b9a42f3c723688dcb4a68805d2"],["/posts/4044235327.html","12da9772de546aa24223683d50df9960"],["/posts/4115971639.html","18b67de349a8a17908d0a5b65fea1d93"],["/posts/4130790367.html","a7ae973ab80881d5189a2dc9d0fb8a79"],["/posts/4131986683.html","56bf922774c965051244603cde1ffb8d"],["/posts/4177218757.html","208680b5df847534e5c0dff3b959429e"],["/posts/4192183953.html","56573fc79dd790f056a1826a779cd533"],["/posts/4261103898.html","3e8cf9c605e8f3ad7d2bfd28f8a98c8b"],["/posts/469711973.html","3f17e257d42a58230a686412756918ca"],["/posts/482495853.html","918f586345271e21782557455557582f"],["/posts/488247922.html","451ce2ad4c9381fbdae1aebfa425ba8c"],["/posts/517302816.html","f341c7de6467cc84393bc6c1df6f7496"],["/posts/570165348.html","7297d62bb6ed55b61083d0fa514b913b"],["/posts/595890772.html","e7738ab2a2d141011d0cb403c3044592"],["/posts/67485572.html","975d4460ed2752270b1f18d73aae5d15"],["/posts/694347442.html","d14c7699cee26d979a8659ba6a529e5a"],["/posts/707384687.html","201bc9fcc24ba5fcf5d10b1bb23e1994"],["/posts/71180092.html","4884efefc384bdb3595e7031f3be5eed"],["/posts/716459272.html","3f1b31c66a55c2cd4e020a680b851687"],["/posts/778231993.html","d0e0367e02c4e131e009bb0c63739f41"],["/posts/795397410.html","fd4f858ff186e4cfb0957c7a25365551"],["/posts/820223701.html","4e3bfa61ae193562dd908a1733d37e0b"],["/posts/830372185.html","55c485ae0144c5bef0d9db41a7657628"],["/posts/88294277.html","6b15a5351bdcbf3217209c26724f4536"],["/posts/939963535.html","515246d1db6805d746fd14a85c0baee7"],["/posts/983786067.html","d4fbd081f281bd62b3e7e2746b8f3638"],["/sw-register.js","180a389f3d615a41d3b8223c5c7db0f1"],["/tags/C/index.html","4058bc4cf6d05f564a6db09e1ad15e94"],["/tags/C/page/2/index.html","63f1c1741e537e0a9734b172583e9f37"],["/tags/C/page/3/index.html","1657cc496d6622cf7a229512400d05c9"],["/tags/ETL/index.html","4cb80a5ec6f6987fd5e8e8c041a55173"],["/tags/ElasticSearch/index.html","e10108f1843c93804ba8cbd4f3dd6eca"],["/tags/GUI/index.html","67fcfb6966119286f7a2600c05e1beee"],["/tags/HBase/index.html","7433f484cb8ddc3238581e1f4480b70d"],["/tags/Hadoop/index.html","ed7d047e4d3eb4b0745846c486d21129"],["/tags/Hadoop/page/2/index.html","c11497da54877521b9e49741822c6637"],["/tags/Java/index.html","4881b30613675ef5faeb8cec7207708c"],["/tags/Java后端/index.html","d24e798fc27b0c7d75cea13d9ee08dff"],["/tags/Java后端/page/2/index.html","4a7d5bb745e4be53696a90807163da54"],["/tags/Java基础/index.html","6f5841708fc387481dfb51c79decfefa"],["/tags/Java基础/page/2/index.html","d8e6a4ce50eb3fcbf7c679d30dcdebaa"],["/tags/Kettle/index.html","4c45ff63215c6b8009bb1f969e5eb6bb"],["/tags/Kibana/index.html","1617ae8f6a04b2e42b3b9848a53a39ee"],["/tags/Linux/index.html","c8cc43ee8d3ab2e0c3a46018070f621c"],["/tags/Linux/page/2/index.html","1fa5efef87de1b33f87b98d41824f8ea"],["/tags/Linux/page/3/index.html","4799474498d57e4c3738126e96257eb3"],["/tags/Mac/index.html","7f816c3471a759e83a5441ac31b2c2dd"],["/tags/Mac/page/2/index.html","8bd34b260f652a7d793c34f65c414e25"],["/tags/Maven/index.html","09325556217c04e3c908e1ef2f8ced9d"],["/tags/MySQL/index.html","19dc8c24a84e130848c8b04987c6f440"],["/tags/Python/index.html","fb404f74d1acaa9ee4918f49b7917b16"],["/tags/Redis/index.html","c3203800312b1175d001f753ab7bc23d"],["/tags/R语言/index.html","09c11830aa776d938f94301877e7ef06"],["/tags/Spark/index.html","4313f99099f65c1e3ba9201202d38e5a"],["/tags/Ubuntu/index.html","4fba5427a16037c508e58104817b8257"],["/tags/Vue/index.html","f58ec49fb6314a43e143bb6cbb745902"],["/tags/Windows/index.html","8bfbfccc5297cabf788ac287c1f1714d"],["/tags/ZooKeeper/index.html","b2d9c6ceff7e88ccaa9289eb76d8a026"],["/tags/bfs/index.html","19cb6df4c8dae58d05ec13c7274ea733"],["/tags/dfs/index.html","6833b1c6c1162809a7f2bc4a86665824"],["/tags/folium/index.html","c32ba21341123b41c12abe313457218f"],["/tags/git/index.html","79c183a85b6148ad354393c2d2a2d729"],["/tags/index.html","55945cfc7dd487aee0cfb7eb20f184e8"],["/tags/latex/index.html","bd300c9739b40ba9d3e547a79fa2a02d"],["/tags/中间件/index.html","9d8e6c53f7458a6b55473b76bed1a05c"],["/tags/二分查找/index.html","dfc3c872664452a84e8f674ecb74fdf0"],["/tags/优化类/index.html","1701a30fa8d3fd6f7944a029767877fa"],["/tags/前端/index.html","ebc188a68ed5de7bd26f3e3e37816766"],["/tags/前缀和与差分/index.html","25f20891352d0a89c1388ee436a7eee5"],["/tags/动态规划/index.html","cee4458f574ec493801621eed7e9c1c2"],["/tags/动态规划/page/2/index.html","6410960102b3e722a089ba5528b068dc"],["/tags/博客搭建/index.html","718144f0613dcc401816b4ef568735f1"],["/tags/图论/index.html","300257d57f345eb0c96c48778a986b76"],["/tags/大数据/index.html","58c171401503209c6d6e875431646849"],["/tags/大数据/page/2/index.html","d761e75316fbf16974145de1f08bb2de"],["/tags/操作系统/index.html","67c83c65d05d438b0bba11193e5b015f"],["/tags/数学建模/index.html","4a1b7c1726d41e9d12631b6af8781d18"],["/tags/数据库/index.html","90c1f222912f411a9be0fc7c84242c0d"],["/tags/数据结构和算法/index.html","031116a1f4e419a36b9530857fddc7fd"],["/tags/数据结构和算法/page/2/index.html","fe8460dbfaf5bdb93b69eb2d86ddbb18"],["/tags/数据结构和算法/page/3/index.html","1d1d1cac324a08eafae6708544c7a4c6"],["/tags/数组和字符串/index.html","553297c38790cd9ce0e04f8e6fef5a47"],["/tags/枚举类/index.html","992eef7ae0490ea2fab9b65a41a3c4aa"],["/tags/栈和队列/index.html","d98d24220032efe979c10566da47130f"],["/tags/树论/index.html","331287c00855b3bd7b8b95412ac5b96a"],["/tags/测试/index.html","fc3374185936d917a78af8ba49883cb7"],["/tags/环境/index.html","ad19dd496fc9000ca0e24d39a7940dc7"],["/tags/环境变量/index.html","5bcb15ab48e2c72a9ba883605a10ff5f"],["/tags/绘图/index.html","341f2024447d9469bb5223b310a44798"],["/tags/编程工具/index.html","2989a81230711af51c9112abcd34cbcb"],["/tags/编程环境/index.html","bf01af9e2cc70d4899ef0b0a191029c3"],["/tags/网络编程/index.html","80b00019d0d5583745bf6076ad006eef"],["/tags/英语语法/index.html","b4db6a2bedcd8ca9f0356f301b43be28"],["/tags/论文/index.html","27bcf22909c04416e0bff8f7ad80e247"],["/tags/资源下载/index.html","4830d142019c01d684211c33caf268da"],["/tags/链表/index.html","39541d61f67222e9c0a70fe29fc2f479"],["/tags/集合/index.html","4462ae6a565d5b20ad9d315e99a540a3"],["/tags/集群/index.html","eb52a2a626d0b84cd8d87ee9b3dd2dc3"]];
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
