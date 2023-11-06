/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","dc1057f1773f4dac57c387fcaaa93cd1"],["/about/index.html","347873c4b39956c853f4bb843c713127"],["/archives/2023/01/index.html","28f8d6d6ec971a0dfb18ba411fe68354"],["/archives/2023/02/index.html","29d6a26c99db21bcebc06b2daaeb3f18"],["/archives/2023/02/page/2/index.html","6b8a99dc370124c3104b0ba2d73ad526"],["/archives/2023/03/index.html","b085f7d0e49f0587af48919ad2624107"],["/archives/2023/05/index.html","4f9d941a29994f7d1bfb5d2f9e7351b6"],["/archives/2023/06/index.html","0b8b33bbb7e46891db53769046473ccd"],["/archives/2023/09/index.html","943a8fcdcea69fc771c361c9b3055d8a"],["/archives/2023/index.html","add65597e76b1a59356454299f8a641c"],["/archives/2023/page/2/index.html","c638e83fce808797bba3ba36cea1c084"],["/archives/2023/page/3/index.html","6716a392be89b3328952e2c8709b7244"],["/archives/2023/page/4/index.html","fca73fd98f41c2cb52f87e08b0e163f2"],["/archives/index.html","b556a053e0e3f80a796d6bb02976aa82"],["/archives/page/2/index.html","c0f20ec70432fe42a538145529c47c8a"],["/archives/page/3/index.html","3c2a50670db1d9b2baf2dcc0452df1b7"],["/archives/page/4/index.html","7c97f45081c2e50a8cdac5bd17db7109"],["/categories/Java/index.html","ef2f61f42ca80157860e3f8a4f353d23"],["/categories/Java/后端/index.html","269fe7f9baa57bc8e78c47a6ecc33904"],["/categories/Java/基础/index.html","c12f468e0699aa5d9c1fa98e23e24f3b"],["/categories/Java/基础/集合/index.html","0a39fe58d174e3827b54a95f73c77c79"],["/categories/Python/index.html","6fdca51ff098cf44444e1bc287e5f5fd"],["/categories/Python/编程环境/index.html","82d9b198cc6c80d3451bbfb520af5e44"],["/categories/R语言/index.html","3913d5c199eaa71287dc6e91da709ab2"],["/categories/R语言/编程环境/index.html","2bdcb95fa4a131582a589b772c9cddd6"],["/categories/index.html","5124debcdc36fd8bfdb6d5d2ead067d8"],["/categories/中间件/index.html","5b07bf20f09d5ac037c0aee65d7ab080"],["/categories/前端/Vue/index.html","6653d753e4ca595f5d81458115bc84f7"],["/categories/前端/index.html","e2d80d1f9109945161038af937125a61"],["/categories/大数据开发/ElasticSearch/index.html","4a70497332ecbe6c66144e3dc0f2788b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","88308a6d066da07d3766acb02fe66335"],["/categories/大数据开发/HBase/index.html","06346347862cff60824bdb745f514f86"],["/categories/大数据开发/HBase/学习笔记/index.html","8776a071ffc3e69bd7435982b8c84bf1"],["/categories/大数据开发/HBase/环境搭建/index.html","b4147a242c68d11b5f8827e391428f69"],["/categories/大数据开发/Hadoop/index.html","abd1236adcb2825aa65f362cb660502b"],["/categories/大数据开发/Hadoop/技术/index.html","03eee67a18b75c9d29cba5e1340f0dfb"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1c8794e8e3e99df4d4372b01b99fb471"],["/categories/大数据开发/Redis/index.html","2d38192f31b4899ac1752b62a2beabae"],["/categories/大数据开发/Redis/技术/index.html","81aaec610cb76ee3d343afdbaacff1b0"],["/categories/大数据开发/Redis/环境搭建/index.html","9261f09fe2d4a88680cb288864378501"],["/categories/大数据开发/Spark/index.html","3c99c1aa01471dab45ffdda057aac9c0"],["/categories/大数据开发/Spark/环境搭建/index.html","59334fc2b46bef13c1e2b56837a861f0"],["/categories/大数据开发/Zookeeper/index.html","000acbfa241f12100faf96777a5df3cf"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1c1bc5aeead793afe95298139a8e3d3c"],["/categories/大数据开发/index.html","4a4e53d4091ca2de52dee5ac522faa5d"],["/categories/操作系统/Linux/index.html","92bcbe158e8d527cb458d3b957b591c8"],["/categories/操作系统/Mac/index.html","c4a7cfc8ec01987fe38965cec421cf85"],["/categories/操作系统/Windows/index.html","3189dd66afa91273fd2b150e364e6d8a"],["/categories/操作系统/index.html","30644f748497bf02f21250293e03d3b3"],["/categories/数学建模/index.html","36dc74385fdb8a533c195d54bd4c737a"],["/categories/数学建模/latex/index.html","979cbc16419c6ae6bbbaa883bbc8f276"],["/categories/数学建模/优化类/index.html","0cc8447d631661f64e89884d2bfcbae8"],["/categories/数学建模/优化类/现代优化算法/index.html","7bcb51ea8bd483916f835294b17a4eec"],["/categories/数学建模/优化类/规划类/index.html","cc7c17bdabd46e6a6cd297f796b19459"],["/categories/数学建模/绘图/index.html","512d21e042b71d4972cb8d90a9ecb79b"],["/categories/数据库/MySQL/index.html","04a2a03a06943b26111629ec72b76d5f"],["/categories/数据库/index.html","efce52b9d811dccb4d30a4b1162baa51"],["/categories/数据结构和算法/index.html","29bdeff12ec1bcdd02b3ebdbe0bbcb18"],["/categories/数据结构和算法/page/2/index.html","f5f610fa771ed46052e0a6e1be62c3d9"],["/categories/数据结构和算法/基本原理/bfs/index.html","d0c73d3f956fe2798d190a5d7f5d09f3"],["/categories/数据结构和算法/基本原理/dfs/index.html","b08a05936c2736f2cf55e903a8b3e346"],["/categories/数据结构和算法/基本原理/index.html","2fb599a24a50f7ca82b897f367062042"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1ce958f17b8e8a0fc4bf11de0370f23a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","eb95dc237cb65b87602bc7b442e5575f"],["/categories/数据结构和算法/基本原理/图论/index.html","b09e1e35d3dfd663bbe4adb51b25fa64"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d589002d649ddb3a67da4cca7ed1f81a"],["/categories/数据结构和算法/基本原理/数论/index.html","29c7bef07b8aeb21e8003a6f28d1e4c6"],["/categories/数据结构和算法/基本原理/树论/index.html","2ae9970ed8fd9cbc84f10f8eed9d2f5a"],["/categories/数据结构和算法/基本原理/链表/index.html","a5e21721e17abea149044edb1c802fc1"],["/categories/数据结构和算法/算法题/index.html","bfd744af13a6719ec40cd4128ea85e61"],["/categories/数据结构和算法/算法题/二分查找/index.html","db7832dd86b6094dff2e00adb63edb71"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9429ba860f9f8042ea7106b63bd26357"],["/categories/数据结构和算法/算法题/动态规划/index.html","cbf5e261371b590a79f576f40ca72d68"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f3e4e16586d179d39fa2fde078764d97"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","553140428873e8d943373c585c4bf31a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e093381bae4d213c2b89b67cf5205e18"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e7b14dfd8a23fde01c5e5d881381b0d0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ee6b76c6e6fc696bafef7c350a7aadaa"],["/categories/数据结构和算法/算法题/树论/index.html","d72a813868583932b89a336039e6fc35"],["/categories/杂七杂八/index.html","b465ce1ed5d4a0e7aea6b27bad9d1707"],["/categories/杂七杂八/博客搭建/index.html","37f58452c56c647f2a417b6f1d3f3979"],["/categories/编程工具下载/index.html","f195952934fd9444c3b7feed3aa35184"],["/categories/编程环境/index.html","4da587fb8ee0dfc184da6e37c16a70bb"],["/categories/编程环境/大数据/index.html","8b34dac5bf65910acce4a6822eb0e212"],["/categories/英语学习/index.html","9d425c0bc4209bec5f979bbb1a20b6c4"],["/categories/英语学习/英语语法/index.html","5f0d485540f9bcfc8a0badd4963181b9"],["/comments/index.html","ee770ee57cda2958c9026a9a9d9fda7a"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","862d1bc4c6f3ba036a1fcdbdb9de368f"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","46b746089dc8d817a61334347228736a"],["/movies/index.html","a28ce54e1b81e02dfc6e65aa476f532d"],["/music/index.html","c30a8e74fdb7c271e70c56912b208a80"],["/page/2/index.html","71284109a03545f5ff3eefaf60792b84"],["/page/3/index.html","b220710bca975a2f648561c256a2b097"],["/page/4/index.html","496068ac8a77efa5a8e40fb420c43b9e"],["/page/5/index.html","48cfbfa7924812ebbfca8e0418654d9d"],["/page/6/index.html","e5e6ab2b3308c72b93a0436a954aba96"],["/posts/1021360842.html","7ed3503548d2d03e85483c68d839b7ab"],["/posts/1120620192.html","6264210d72d4fc18fff0efbfb792d639"],["/posts/1141628095.html","e3ab79d9608ff4b0789064701eb1b38e"],["/posts/1168613674.html","fb6f8ddb1b5977e81b4d4578199df5fb"],["/posts/1219920510.html","c758602fc5b5c4745c92d336295e71c7"],["/posts/1222166338.html","00cbb6da2d51fff9d4d92c485e6fc2af"],["/posts/1259097482.html","51193122ce48c383df956edd7b62a296"],["/posts/1271036369.html","bd6245c1497dddaed004d943594f0acf"],["/posts/1312847445.html","d8f4240fde40a4f11ebdabc325812fa8"],["/posts/135355774.html","d3de0830993697b0ff0ba44b3229e72a"],["/posts/1375344716.html","6db158a806c5159dc9b34f99dbd667b0"],["/posts/1388991698.html","ce108d93654b45ecd27557376c0c2b84"],["/posts/1410315814.html","d4824c285d452b305f31783c2ab2d5e0"],["/posts/1452790229.html","eb73f0bd9657f5d353de8240df23f843"],["/posts/1470079884.html","4f98e0f5a37258a7d2b994fa054d88ee"],["/posts/1470079885.html","a019664893f579ae23526beda544ae4b"],["/posts/1470079886.html","a460aa7f9311fa34836f1291eb5ce2d0"],["/posts/1470079887.html","83560a71e0914c516d9373291ca68b88"],["/posts/1498536549.html","2815ce878a1b3d5e301ded1d8048e249"],["/posts/1547067935.html","185c7d42747149d5a6be0b37012d4e02"],["/posts/1557866301.html","3706efe096ed8a0604e0374df4bf5066"],["/posts/1571776361.html","ecfa203319a0e4c60307aab45899221b"],["/posts/1605124548.html","4cf743c8dcecd99859950309888232d9"],["/posts/1633036852.html","174c2228f420f1c0e845474aed233f9d"],["/posts/1674202625.html","da471cbfdb8285176244a9a772d6af21"],["/posts/1765123828.html","89dadef87ebdaeef3e1b34a7b6b848df"],["/posts/1767336200.html","6bf324d51fdeb7a1d74561761b9899ad"],["/posts/1776114197.html","6677b4561aa2f9e233845b29f23154cc"],["/posts/1817748743.html","95ef45f718f91b30db8c916bc2b1573c"],["/posts/1925125395.html","58f7cef6307e7e2c20e14c974069d769"],["/posts/1966191251.html","974502dfae12ce624c8e31f53c4a2cc6"],["/posts/1987617322.html","150ee8f3a19a5e7d99a38f4a0a3af437"],["/posts/1999788039.html","a41f56ab856f07b6353fd505b0baee7a"],["/posts/2075104059.html","8f6e031ab04a196cf2b62ad981c4b0c2"],["/posts/2087796737.html","b25f50a7e77f9b84bafd3d199be8eae9"],["/posts/2106547339.html","23445ac4213aeb396682e4e137283370"],["/posts/2207806286.html","e77e42ec923b6604aa58eb2b77c17a65"],["/posts/2225903441.html","5e2be1237e3476d62b3baf2ae6dbdbe8"],["/posts/2265610284.html","655360b0de61ea3b16156c69dba7949d"],["/posts/2281352001.html","573a09e49b4f775a42aaa636f5b77006"],["/posts/2364755265.html","95575423befe1cba9bfc6c3de22cadbc"],["/posts/2414116852.html","97af9a57705e86df39fb7349a7f8abd2"],["/posts/2421785022.html","2595983be1e2c76773906af43b5ed2f6"],["/posts/2482902029.html","76fffcc4286e09fbb01695fdc271a227"],["/posts/2495386210.html","47f78510d292eb7be14bcb5a6bfbd160"],["/posts/2516528882.html","7b4e943aba09dd4430cdda55f5e104d4"],["/posts/2526659543.html","08bf355ae55a111a6fd7ce5316625240"],["/posts/2529807823.html","635fef318ea99e9cd118dbb808bc4e60"],["/posts/2596601004.html","b0595e76221aa501d54543c4ae1fada4"],["/posts/2742438348.html","9c3b65c30f16b67c50b33701706db830"],["/posts/2888309600.html","8227a52043124798e145aed9f700c7d8"],["/posts/2891591958.html","1b5e585f6392dc748f7c5eb39c875064"],["/posts/2909934084.html","043909c976936837900abf5524d5bfa0"],["/posts/2920256992.html","df998039ed104f31235a929a96408dbe"],["/posts/3005926051.html","c3cbd5e4f06ba6051b09f97eadd39faf"],["/posts/309775400.html","6946860c3e899bca7cb87c05783d2808"],["/posts/3156194925.html","79b6e4d68bb95ec8c29822773d8a9e98"],["/posts/3169224211.html","04ee3e472e23c79f912eb4deece6647b"],["/posts/3213899550.html","f94430b26ed544ab36caa8e2bb55f665"],["/posts/3259212833.html","c004c5244d8c42126b6204b71b879d84"],["/posts/3266130344.html","a2fa2e4cfad6bfdfcd01724ba8bc1998"],["/posts/3292663995.html","22deaa4478701994d3152179e4de0ae0"],["/posts/3297135020.html","15c0708916c1ff2de4fc79dff7276c06"],["/posts/3306641566.html","60c102ece6526eb9212f46694c2680ce"],["/posts/3312011324.html","79906aef8d89a65b1f3ba0038397d077"],["/posts/336911618.html","523debd16bd5289e14483bba8f5cd3e7"],["/posts/3402121571.html","664aae0dc472a7f8ab0d72b29a06c3ff"],["/posts/3405577485.html","770f67fa27d91d46b4648e68699dbe84"],["/posts/3498516849.html","009a574a4dd0f907acd0ad21df33dea5"],["/posts/3513711414.html","af6f9a1590ef67d74b99142b66503fb2"],["/posts/3546711884.html","8bfbaa2ca4cc5d58ffc9aa0c8dc31720"],["/posts/3731385230.html","a21b131564d43f5b3a47c04b8eef177a"],["/posts/3772089482.html","f0560bfae9ec1630225d4cafb00d2024"],["/posts/386609427.html","233c6f034bc8ec29b159e69db910c4f3"],["/posts/4044235327.html","cc433e46ac1456615cd966754b912fa9"],["/posts/4115971639.html","d9e7c84efb1e38e62f3a90931fa397d0"],["/posts/4130790367.html","d00ae6b85f197b099720916e239056f7"],["/posts/4131986683.html","0acddf3db4b4a383b5d4aca7243eb861"],["/posts/4177218757.html","631263749e35c099c36e83623b7c428e"],["/posts/4192183953.html","9a9d8b683190c63f68d9e18b970a547b"],["/posts/4261103898.html","bd35c3f4ee81e99eb7520b6a76d3de24"],["/posts/469711973.html","aba20158178f08d3791429a0a559cd1b"],["/posts/482495853.html","f9319b59a1ff452d10bed0e04ec4ac04"],["/posts/488247922.html","fabc53d287c01378354fc11f9d7c60fb"],["/posts/517302816.html","ecb90ba49b6a6d9e568cfe53f0bfd723"],["/posts/570165348.html","38d073c9709cf20b51e2e1319aaab35c"],["/posts/595890772.html","23d28e0d5855011972637f9be3681bf6"],["/posts/67485572.html","751e326df4480ae0682111c2c03c7224"],["/posts/694347442.html","cdce1751a3193990dd5f6321d2518cf1"],["/posts/707384687.html","ec2c425e5a7c4969bf2329399adc0baa"],["/posts/71180092.html","6314b33b93309b7b2c46c32ba80c3aca"],["/posts/716459272.html","6cd241f34f713ada23901c750819e898"],["/posts/778231993.html","cffa88412b7d639a33d1290adaed1738"],["/posts/795397410.html","f45f2813c80c6b085ef2e2276987ca91"],["/posts/820223701.html","21773f9501fa4af62f04203a0c19e778"],["/posts/830372185.html","65ed73982fa73bc56777d6727e1f87dd"],["/posts/88294277.html","a055d0deca56cb03be5b4982a91347bc"],["/posts/939963535.html","29281c262ab5a01077516c89c79cbb07"],["/posts/983786067.html","87dd2e9e6cb3b3308f4363ad792d6917"],["/sw-register.js","1eaeaad009e3fb0f6d45dacfaebe5b9a"],["/tags/C/index.html","00af0816c6d9d143d37177a00ecb4714"],["/tags/C/page/2/index.html","0decf2a9436298001b1516df47e2884d"],["/tags/C/page/3/index.html","169d1e8fd4cd106735fe171bb1b4f33f"],["/tags/ETL/index.html","ea93c3396b2be09b5e651c48f6bacbb6"],["/tags/ElasticSearch/index.html","26203973562628815bcc61f209c4ae28"],["/tags/GUI/index.html","bea625b4a4ca09b5e9f5bf5c03e9de2e"],["/tags/HBase/index.html","1ffcbf8d94e50a15438bb9bf22cd6958"],["/tags/Hadoop/index.html","f493af5ecc08478b03ad4429cc966178"],["/tags/Hadoop/page/2/index.html","3ac4e7ce9cad3998194105ca08e0f557"],["/tags/Java/index.html","4cf1bd99dd25dca79770ea36592c217e"],["/tags/Java后端/index.html","2b6bf692367bf6af221ab182cd98ee40"],["/tags/Java后端/page/2/index.html","ac9f022336f85b4a1a94d44c26ad7a91"],["/tags/Java基础/index.html","c9ba8243ffa36eed9430f9cc500e8a4d"],["/tags/Java基础/page/2/index.html","2c0173c332ac76a5bab427df4005c1d9"],["/tags/Kettle/index.html","c1035ec87cc006e644a909c4bd1720c6"],["/tags/Kibana/index.html","4e168e3511c3a1285a8426578950b35e"],["/tags/Linux/index.html","e954b05093571e4184ba55599f3ba0e4"],["/tags/Linux/page/2/index.html","cb3e9db802785553d1b98c4a02d0c27e"],["/tags/Linux/page/3/index.html","8df234d672157b29a14c141abfd27679"],["/tags/Mac/index.html","36f346e14bfb35f82eeef0da647e3efa"],["/tags/Mac/page/2/index.html","5380577c58bb60291e1ab817a1f34f4e"],["/tags/Maven/index.html","0f8a4a83f07cf606770329dac19d57a9"],["/tags/MySQL/index.html","607c13ec89b8c5338ac9270f6265dc6f"],["/tags/Python/index.html","61bdd7737f3a176cfce358a798c1b374"],["/tags/Redis/index.html","c689d79dc2a8f09eb31372ee6a96fac4"],["/tags/R语言/index.html","7505200a544bf8d61608898b6decd459"],["/tags/Spark/index.html","4342e35ea4d9964c6c74ea33828fd781"],["/tags/Ubuntu/index.html","406a96bbddff8313ed1021d148c17f08"],["/tags/Vue/index.html","e6b3bea8dbe9d551ccd0cf50adcc343c"],["/tags/Windows/index.html","96196f33212a2acefea4fd97c3c081eb"],["/tags/ZooKeeper/index.html","8bb0e0a35d848145df55cb2b1b32a232"],["/tags/bfs/index.html","6410cf09e0029f857d0a8010b064bc6c"],["/tags/dfs/index.html","d8c0fcba506ba27304d32f687e6f1cbd"],["/tags/folium/index.html","1090e1d8a14ab87e944bdecc74d2b1ac"],["/tags/git/index.html","3510e08a63410d413b5b721083ac3cbb"],["/tags/index.html","d565cc27f6e4b632c09fe582f4dbf27f"],["/tags/latex/index.html","1dc5ab0189ebc3be853f969db7e1c308"],["/tags/中间件/index.html","fd68bf55295000cd30e937182cbf561d"],["/tags/二分查找/index.html","d206f9289acbc5ea986642813f98bf0f"],["/tags/优化类/index.html","1fd80f349af6aa63fe93ade5d2f308b1"],["/tags/前端/index.html","58b709acee520c23e4400983d255e108"],["/tags/前缀和与差分/index.html","e2588955a4ada884170a50986b754843"],["/tags/动态规划/index.html","4e1923b9d84ea5ff2abf7d43c0c9f2b3"],["/tags/动态规划/page/2/index.html","7a3aae77878c92e592ebd1f7a5b1a553"],["/tags/博客搭建/index.html","0537412b5c846bb03f37a7c34a23e9dd"],["/tags/图论/index.html","8e6fd03a9e422d0b74d2df66200e12c1"],["/tags/大数据/index.html","ad977ef44e081c1221cfb3deb2290346"],["/tags/大数据/page/2/index.html","bd1be2afed98b96c9c5294b0a8e5911b"],["/tags/操作系统/index.html","a1595040fb072498d471074ac4b38011"],["/tags/数学建模/index.html","d85354cfcf4ac53c8fab447b2f7e0dc0"],["/tags/数据库/index.html","e931b399df5442091c67fc9260ecd1e7"],["/tags/数据结构和算法/index.html","3c2417a4e82bb2d3470cf01566e73579"],["/tags/数据结构和算法/page/2/index.html","8e58e1a8e1461c78411ad23a93f7b876"],["/tags/数据结构和算法/page/3/index.html","7da00429e10f9a27df49a991fda0f3eb"],["/tags/数组和字符串/index.html","e60ef65d4aaba10fa2e823f48d52d1aa"],["/tags/枚举类/index.html","2fc1e38a4f18aa6aef9f87109dc5f9ef"],["/tags/栈和队列/index.html","799ac25ce97efb1075c4336fa6ce99ba"],["/tags/树论/index.html","25e11f5e94910eb854c5bc3bb2681afa"],["/tags/测试/index.html","8a57ddaafef789def98ef4945d04ba76"],["/tags/环境/index.html","1b2c0512b8619f0a3d245f0662357023"],["/tags/环境变量/index.html","ba461f16f6b4bc74dadd9f87ff245714"],["/tags/绘图/index.html","6bb1a60b51a59bbd02258e0103185b15"],["/tags/编程工具/index.html","f3c20fa8dba5e7b24b3481e72c90011e"],["/tags/编程环境/index.html","5f9bdbbfba53e45db2caac1b7d08081a"],["/tags/网络编程/index.html","c8b685eff0d218e26874f4b5ed1252b3"],["/tags/英语语法/index.html","279748da259645fadcc280815b94d862"],["/tags/论文/index.html","c1fa1ff8a64e7c9db317d41ab5d4e79c"],["/tags/资源下载/index.html","e2f724742a4dbf838c96720a45234bf1"],["/tags/链表/index.html","e26010cf1dee81ae1a9904b3255205d0"],["/tags/集合/index.html","43e2b40edbda55d22c5f7f722c3c2c3d"],["/tags/集群/index.html","82954e5633310298aa51301b9595b669"]];
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
