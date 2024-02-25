/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","0be113f42f44672e51c8e81ca02a4508"],["/about/index.html","8d043c637c1b71f7318bd8bc84acd0f1"],["/archives/2023/01/index.html","f0165b3f5ffd616dd05628a98ffeeeb3"],["/archives/2023/02/index.html","16f26e30789505dd68e363ea6a7e80ca"],["/archives/2023/02/page/2/index.html","5a4c122dc83e1efd7cf545163cb8e7e9"],["/archives/2023/03/index.html","c0c6b4546a4afe9b24bbd846667b9a9b"],["/archives/2023/05/index.html","8cf6f5d584966bb1b3a70e0be42cc871"],["/archives/2023/06/index.html","94129c435d5d8450182e1ed8fddac1c0"],["/archives/2023/09/index.html","aa58af1a7c6915b881ba0906fcfa56f3"],["/archives/2023/11/index.html","bc58968071cf466038329fae1a14f9d5"],["/archives/2023/12/index.html","da0469269a3e836102a8000afc2991d3"],["/archives/2023/index.html","fa12fdeca565a59650dcdd608634af4b"],["/archives/2023/page/2/index.html","03607b91617cd53ac852ade018da0351"],["/archives/2023/page/3/index.html","e4978a203cb3820f21d4dbbdc8223efa"],["/archives/2023/page/4/index.html","a6ebf9cac4718a455d5a561eaa71714d"],["/archives/2024/02/index.html","af4ec8adb30529e834b748d682c19f78"],["/archives/2024/index.html","3f16ea4c6c84e4ca0c40e4d084b7a721"],["/archives/index.html","e9490ea95183b9a7b0a0ee345c6930db"],["/archives/page/2/index.html","efd4c1abcd588e9c07b164169b296589"],["/archives/page/3/index.html","98ec8e72815588030bb09b37292d4a4f"],["/archives/page/4/index.html","1375c73a3cefac3bcc0ca50d7cc23158"],["/baidu_verify_codeva-qQP2iZOMLX.html","cf116cc18d96f751b8f0175e543aa6f9"],["/categories/Java/index.html","4cd8fdf5c96df51d2729605084d7b8b1"],["/categories/Java/后端/index.html","817b3389c345e6d8d177929c7d2f5ba0"],["/categories/Java/基础/index.html","4035694feeaefdbe526d59fac32aaf1a"],["/categories/Java/基础/集合/index.html","c097911845ecf7ecfe7a7e7e76e52edb"],["/categories/Python/index.html","9b4a744f5848b68650bd99e08f8ab87e"],["/categories/Python/编程环境/index.html","daaa34c5e896cf623597733244f794bd"],["/categories/R语言/index.html","a398b34e00660dee611c0d3e972f3572"],["/categories/R语言/编程环境/index.html","e6fbc09ee3daa0859d9a09e61c3443be"],["/categories/index.html","b654c0ea6ff62903b1b01cf7bd87662c"],["/categories/中间件/index.html","26d571677847bc9f0c578bea20055f9e"],["/categories/前端/Vue/index.html","244aee396131b6f4cf733f2a486fd1d8"],["/categories/前端/index.html","ae4b271db5681ae11d1569421493d4a3"],["/categories/大数据开发/ElasticSearch/index.html","ba66557a3a4b3400a166cd7f7117cac2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e5324aea68e48353497c4d249a458f82"],["/categories/大数据开发/HBase/index.html","3bbc169a49159d199ac92b71edd7c5db"],["/categories/大数据开发/HBase/学习笔记/index.html","ab75ee1f741f00a499c07f9852e05825"],["/categories/大数据开发/HBase/环境搭建/index.html","52b58e4ed716a7e73d21cafb631af489"],["/categories/大数据开发/Hadoop/index.html","ff160ecd08676de7e4a561773ddb23a5"],["/categories/大数据开发/Hadoop/技术/index.html","5fd609fc7186df0707b4ec60290dfb60"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b476997a0dbe9b1d3be92559ebaf045a"],["/categories/大数据开发/Redis/index.html","3d1c635b40358e63b0b5bb26f34e756b"],["/categories/大数据开发/Redis/技术/index.html","74210c9f691ef39a4c5edce21922491d"],["/categories/大数据开发/Redis/环境搭建/index.html","e1be30bfed627ae3c05ee88c35711cb0"],["/categories/大数据开发/Spark/index.html","c4659bb550054b2fcece3fc31314eab9"],["/categories/大数据开发/Spark/环境搭建/index.html","4e86fcc098ff9fa2f6b556f5e3321763"],["/categories/大数据开发/Zookeeper/index.html","f9ad31eff85b8c0c925d9e6ed0d03a1a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","42edea2eff6f313bec418236f4d846b7"],["/categories/大数据开发/index.html","a94dbee652f36e5fb8b9aa3243cc0bae"],["/categories/学校课程/index.html","0acff206ecaba599ea62a3a9c9a92b33"],["/categories/学校课程/计算机操作系统/index.html","76e34b0158cb57439b180ba9d4dc07bd"],["/categories/操作系统/Linux/index.html","0f11a9b17e0ef785db149025204cbdbd"],["/categories/操作系统/Mac/index.html","dac47b20cc1914b1977134106e517ae1"],["/categories/操作系统/Windows/index.html","611b78151aa1b3efbf2f91530899e650"],["/categories/操作系统/index.html","cf9577b572ecc29705c65f75c422769c"],["/categories/数学建模/index.html","33b60e2257d065a74f12c915691a86ad"],["/categories/数学建模/latex/index.html","03a6707d5f8e10975891cd31c8c1544b"],["/categories/数学建模/优化类/index.html","f106a17aa458b77ce42c9b44770fd53e"],["/categories/数学建模/优化类/现代优化算法/index.html","94cea80b23bfab44e9ad709941bba0bc"],["/categories/数学建模/优化类/规划类/index.html","5ac128b809e1a1bd58ddf8552a3968e1"],["/categories/数学建模/绘图/index.html","9068c79acfb7503757d47a36a0ab6c3d"],["/categories/数据库/MySQL/index.html","5772a6830a53d3ee7b645d404c894084"],["/categories/数据库/index.html","6c09d9763e0e7cdb4ef4e8179549de68"],["/categories/数据结构和算法/index.html","d92c219541ee57e06b42ac0bf8ceedcf"],["/categories/数据结构和算法/page/2/index.html","26af95aeb4809a1e3de150db35194aea"],["/categories/数据结构和算法/基本原理/bfs/index.html","0169c35b157ec66880ccdfc410f43ea0"],["/categories/数据结构和算法/基本原理/dfs/index.html","0ddee465c61de33ffc0214cc327782a5"],["/categories/数据结构和算法/基本原理/index.html","9a8eeef98325de901a0a36853d4801c3"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4255c5bca7ea906e0b822a6bad9cadef"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3ca853a92df4f20eaf655c7d5e9f4de7"],["/categories/数据结构和算法/基本原理/图论/index.html","823dbe1952f0bf99812608a5318d206c"],["/categories/数据结构和算法/基本原理/字符串/index.html","9e4a5d9dfa6eed345a353c43aa848236"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6b85ebc97ac22d304a532ff474d29531"],["/categories/数据结构和算法/基本原理/数论/index.html","1ea832ede5afc122f41e68f824be5b18"],["/categories/数据结构和算法/基本原理/树论/index.html","2b03487be18b7fed4987e28297da3836"],["/categories/数据结构和算法/基本原理/链表/index.html","9a08d6e64aa3dadb49aa40b497b4710f"],["/categories/数据结构和算法/算法题/index.html","65bb80b9c264c1a0b149d4a7742810c6"],["/categories/数据结构和算法/算法题/二分查找/index.html","3615a325a5c68d99fc545573f02204c7"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2dd88f80eab45bb7b94d09132c8313b6"],["/categories/数据结构和算法/算法题/动态规划/index.html","3fc49fabd9bf18a6c8cd9256ec5c4a3e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a8a403f8a9d78d6a0be9ff36e437cdc9"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","05c95b7408b17e9e30982c77250bd415"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","dd9c170338abd1556d3bf547e457f8ba"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","6a3f8510349f70232cfad5f5d1dcf9fa"],["/categories/数据结构和算法/算法题/数论/index.html","2fa986049a809c78f8936aa7df85d1ab"],["/categories/数据结构和算法/算法题/栈和队列/index.html","dd999b67e846f8307828c752361adc0f"],["/categories/数据结构和算法/算法题/树论/index.html","2132a25700ac6e6e03d5aefebdf7a426"],["/categories/杂七杂八/index.html","6b3bb69f9cd14cec3f2f2eb157b73398"],["/categories/杂七杂八/博客搭建/index.html","816f955442e1222dd0127898ff5f3ee4"],["/categories/编程工具下载/index.html","cc2a88fbf21fdf1fbefb153a300a2cc3"],["/categories/编程环境/index.html","87e74b41473a83538e2547de93c061ae"],["/categories/编程环境/大数据/index.html","c7cd24add32793e73385b5ba5ba91b15"],["/categories/英语学习/index.html","6a9d139c5f2482b4345802b42293779e"],["/categories/英语学习/英语语法/index.html","5028a2f1fb907852b4af1a821373e5e1"],["/comments/index.html","cdd5b313adcceb09ba52013aca720530"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2f2f76f56d566f9d2577b985df050d04"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8738740525e8f259e8a1ca7769d6def2"],["/movies/index.html","6b6439f3e058fbddc91e0ce1bbd53079"],["/music/index.html","e9c97a6e4b848ac781353736c7c6c669"],["/page/2/index.html","8bc517ffc57dfb2819aa6052182b34b7"],["/page/3/index.html","a7aaa41351714a1eba5d0c3bd6a29409"],["/page/4/index.html","f8c599178b82ec8605a7ce8509fdd6b1"],["/page/5/index.html","39d1a2655d8fff19d09a8f964d268dda"],["/page/6/index.html","c4ecd90a053af2506ae68f887112b1d0"],["/posts/1021360842.html","1de4391a4dc18a9535b254961dcd8653"],["/posts/1120620192.html","7cc978d37360d38bc59ab0aff4e5ae56"],["/posts/1141628095.html","844ca485e95d478e085593cd02dd9aba"],["/posts/1168613674.html","f6f582996824304ad8fd6b62b324d280"],["/posts/1219920510.html","d327291a902f8db45d14f5fdc550b35d"],["/posts/1222166338.html","495431dd0e65934ce926e83bd3457600"],["/posts/1259097482.html","305eb56058ede065f9be8d2c3357cedb"],["/posts/1271036369.html","dbb474979c73b51a21e32d921b210de8"],["/posts/1312847445.html","b0b92d05b666b74f8afe6599d30b81a6"],["/posts/135355774.html","31759e84f7004e03d78ee3200838a933"],["/posts/1375344716.html","1a583913a5b3b35ee010e7ba905494b8"],["/posts/1388991698.html","ce9ed0db125386c65b349216847ff092"],["/posts/1410315814.html","7541e884bd2be3b3272141bee3db6710"],["/posts/1452790229.html","45f05011c8758816c1c264ad1970a42f"],["/posts/1470079884.html","3606c01372afad51ea40b97b45bc6fa3"],["/posts/1470079885.html","58f3c7e33e50deb28331629260a7f3ab"],["/posts/1470079886.html","bcf12d5e15a8b53949274b21d166a514"],["/posts/1470079887.html","f78fb8351c0e03133a6f1f4a535ff762"],["/posts/1498536549.html","ba017445ebad65877cc32ae6922525db"],["/posts/1539568593.html","6f9012ee932797af5c7c1831ddb0e1e4"],["/posts/1547067935.html","d6db0bb64e2491f511ad30e405216f06"],["/posts/1557866301.html","e44e1052e75b758bddc4f6ad906c5f78"],["/posts/1571776361.html","bc23237fcd5f3b10bd18c6de9c37773c"],["/posts/1605124548.html","d130b97b35aff4d2e609f39afdbb07b0"],["/posts/1633036852.html","397d344247a20948911b0c4a96094f98"],["/posts/1674202625.html","14614d088d993e3e44c695e5b9434501"],["/posts/1765123828.html","836246d0fc99695782f019ba180a0a57"],["/posts/1767336200.html","625876c5ba1c29182cff88df36170355"],["/posts/1776114197.html","98641baa82fa02cf8b4c617e5bed2fe3"],["/posts/1817748743.html","26ab28a007524324a98215eba0e55704"],["/posts/1925125395.html","9b218455439902d04dfeaefdd18daaca"],["/posts/1966191251.html","9c6425330ce2659372b2591e138bcf6f"],["/posts/1987617322.html","f511b4c5d0d072dbb81eebfa174b0ed5"],["/posts/1999788039.html","5f2fccd2d13ad524c7288b9eec216d99"],["/posts/2075104059.html","72da10577a270dad242769a40d410e96"],["/posts/2087796737.html","b3b41fc4388e9c4e4ab34ec2b2b098a4"],["/posts/2106547339.html","b7d1de11741beec42cc2206745ccd568"],["/posts/2207806286.html","4b6902ff8c458acf621c3cc0ee989384"],["/posts/2225903441.html","517d29aefc5a32fc9a1f38dcf926bcf3"],["/posts/2265610284.html","1f7d692770fab7e65838b193c999fea9"],["/posts/2281352001.html","0fa8fa21c7f438fe4c4d17c81f112f38"],["/posts/2364755265.html","666ba7f3625d258d3b1f8926636bd2b9"],["/posts/2414116852.html","91afecedbc0ad8d755e01c45df4713fe"],["/posts/2421785022.html","2c66b270ad4d42a28f9ffd781aeea9c2"],["/posts/2482902029.html","9d830574cea461893e9cbbc02d5a7bfa"],["/posts/2495386210.html","7ef716302486d4bc2b20aa4c325e7455"],["/posts/2516528882.html","a3bdb811167d6637f97deb9364eaac45"],["/posts/2526659543.html","6730e4ca956bdd3d1cfcf67066894ef1"],["/posts/2529807823.html","1479d8322b515a3ebdd767141b3ffb47"],["/posts/2596601004.html","91045dbea0c2c297a8c8a1082cfccba3"],["/posts/2697614349.html","1470fb12ff8dc479654ea4f9ed803835"],["/posts/2742438348.html","85f60d9c846f8b7d19e83203e05e6dd0"],["/posts/2768249503.html","e046f55d94a8b82a7afdff6e5bff7738"],["/posts/2864584994.html","1d3833196c01ff87ebbc882008ad05f7"],["/posts/2888309600.html","b105e765af151c819091de69322bded0"],["/posts/2891591958.html","de9c36425e210c6db6f4aa062ca5e30e"],["/posts/2909934084.html","234eae37b92a7164d7d4d771eb1f4728"],["/posts/2920256992.html","804f6bfaf2d3e27068b6a4531429f52c"],["/posts/2959474469.html","51050e99e5d833a329aa610048b0a33b"],["/posts/3005926051.html","55d1aed9780c49a73c0b93dcdb02a01f"],["/posts/309775400.html","fb920d0cba0a41f9441cf95cbaa6e292"],["/posts/3156194925.html","f1fe47ebec00aa94d1acf31400dd1b70"],["/posts/3169224211.html","b981d521b9041e301c187bf9e11afb15"],["/posts/3213899550.html","68d7d12726d442e76f97e30e2b18c46d"],["/posts/3259212833.html","c899288a335d7c2bb40383668941b357"],["/posts/3266130344.html","bfc4ee24b88250da791cbeb42c9cad37"],["/posts/3292663995.html","125910846e8b03f207c4284ccebd0c9c"],["/posts/3297135020.html","727ab10ac9bc7233e433d97c8eab7ea2"],["/posts/3306641566.html","d4db8c182bfdd58ef6e2d8a9c6ace1e8"],["/posts/3312011324.html","a622806a6fac4d78cd237201c0b34c6d"],["/posts/336911618.html","c8e3150a13c2fce77f42a07f0a8432c7"],["/posts/3402121571.html","fc484662111cd30b1c4036e447a05b25"],["/posts/3405577485.html","fc798f2ea072e7f50ea59bb9a7a80376"],["/posts/3498516849.html","fd642e55e34611e3ddbf83124ee841a4"],["/posts/3513711414.html","b3e238a7b129a13db0443b1ed9719a5a"],["/posts/3523095624.html","a1b93aa0a4f5de19991a12f17ab905b4"],["/posts/3546711884.html","d96ff52f03e716419614c5f263142cd4"],["/posts/3731385230.html","b9adc36e5f7a2850949795c5c2602f01"],["/posts/3772089482.html","2879c6cca35b7ed2b36fd5aa375cba16"],["/posts/386609427.html","c1d387976620ed60fc3ba70b16ae4f08"],["/posts/4044235327.html","b2cc3d0ce3c9314f358fa46955187ea1"],["/posts/4115971639.html","e2350ee21cdb216adbe5863f5a25247c"],["/posts/4130790367.html","ea1ada17d8d6c280ed4b2119500759b5"],["/posts/4131986683.html","02cceca8622c58fba329b68a55628893"],["/posts/4177218757.html","d5dd34758cf7138446c6fc0c618b17cf"],["/posts/4192183953.html","fa4af03a7c0341cf386f0ed8885ffb3b"],["/posts/4261103898.html","14046df62ea041392e25cdba30f82da8"],["/posts/469711973.html","bb9c0685a362364ddb66abf8977f20d3"],["/posts/482495853.html","7dffbda973e28b0cda219ecfe52f5765"],["/posts/488247922.html","1b8f91897d4d4fb38b13f1eb8077511f"],["/posts/517302816.html","0ddb05c47b08d3372672c7139f8b8caa"],["/posts/570165348.html","6bebda77613daadca32bad3ec4b020cc"],["/posts/595890772.html","759b0f2eec7c8d38ede687916385e512"],["/posts/67485572.html","2ed725700097d996c6e69cde9f37f0d6"],["/posts/694347442.html","8c527f073980aab53882452ff84f3f2a"],["/posts/707384687.html","94e1fe028b74b30103611b519e3dd00a"],["/posts/71180092.html","9b1960629b6cd08772a18ac339c1c805"],["/posts/716459272.html","88d5b82f77cfcbb7fb8836e51e069f9c"],["/posts/765481613.html","548c8404686f27bf8a942b8e53f12dce"],["/posts/778231993.html","d8b2659f360761db369eda2238f68a8e"],["/posts/795397410.html","7fbe83cf8c393932155d46474a4aa561"],["/posts/820223701.html","9b46d616aef0a3d881ee210717b0a022"],["/posts/830372185.html","8883294ca080d60c26dfd29d3a23349d"],["/posts/88294277.html","ef395a4824e790595aa73512774f20af"],["/posts/939963535.html","ae1c8f73f7572a8f7eed1b98b0febf38"],["/posts/983786067.html","b24f726856c3d5e7efd49b9033c578f4"],["/sw-register.js","098b33cb433855aba5a6e0cb6162d4fd"],["/tags/C/index.html","c48e7280115af85b4ff408dec2bd8e27"],["/tags/C/page/2/index.html","eb39e27201c9ecb0098c08f259ace992"],["/tags/C/page/3/index.html","65f18a739fdef65548a453e2e347742d"],["/tags/C/page/4/index.html","25a538ec21341db7f6c0140811cd0ddb"],["/tags/ETL/index.html","0fe0d2a12d64b8f010582423805bd153"],["/tags/ElasticSearch/index.html","3ef69bc4428f42adfa3a67fc77aa6cd8"],["/tags/GUI/index.html","9d2ba47077fb21326a07032756079af1"],["/tags/HBase/index.html","806ec6645e7022c19edf23eedb1f3dac"],["/tags/Hadoop/index.html","d239cca01e14c5821a541c9ddb39937d"],["/tags/Hadoop/page/2/index.html","e5d19ca69a70456ebab5d6798a42ccbd"],["/tags/Java/index.html","b3cc9aa2c9a197b1478603d50dd5a3e3"],["/tags/Java后端/index.html","ff53194db04376711fc2d2b1b5825281"],["/tags/Java后端/page/2/index.html","f236af5bf60a1dd705efb0a2200ebdfa"],["/tags/Java基础/index.html","19aebe8881d029657863f46449eeb0a4"],["/tags/Java基础/page/2/index.html","f7f064cf77a20af6fbde18f0746f4b48"],["/tags/Kettle/index.html","9d12d3d5ffa18011d6be5b2f92269741"],["/tags/Kibana/index.html","a4f9df77fe04d84e227018e451961944"],["/tags/Linux/index.html","b7ab76764a39e8dbe6547d962fd92f76"],["/tags/Linux/page/2/index.html","fe35822daac42ac4226afee8affebbe3"],["/tags/Linux/page/3/index.html","af4f6b5c82198a208da1d58be97770e4"],["/tags/Mac/index.html","80d96dd3c865e4cd66c46b01fa5721a6"],["/tags/Mac/page/2/index.html","dc2cc006badad05549a0e8642f7b1da8"],["/tags/Maven/index.html","6c5e25db512fe4a1851535a443f5ebcc"],["/tags/MySQL/index.html","07107208c3793157a86e2fe55e0d087a"],["/tags/Python/index.html","eec9bd2dfd97688e85492234b485dc8d"],["/tags/Redis/index.html","e5fee73878c62b08707895dafe0527b6"],["/tags/R语言/index.html","56754803b03877557f5b424548542ac6"],["/tags/Spark/index.html","a306201d54a0a03a4475a72e2063edb5"],["/tags/Ubuntu/index.html","cba7ab585f48c5fc50f912bc1bfa79eb"],["/tags/Vue/index.html","bebba50f342b30dba264175033f05791"],["/tags/Windows/index.html","616665f37557508f4a9c66d468c09a49"],["/tags/ZooKeeper/index.html","2cc6e711cb38cfae05bf28c507398732"],["/tags/bfs/index.html","a6e50ab7bf842f1791b570ef0e33eaad"],["/tags/dfs/index.html","d5a3db0a67d7c43ae38b49cd74406a9d"],["/tags/folium/index.html","86e5cd88f61a6c0973a90501dae3cfc5"],["/tags/git/index.html","644a0c83f1248a8187890617f5d5d92e"],["/tags/index.html","052792c71bb19e7912c38ad6a0b27f26"],["/tags/latex/index.html","b957059b7ee72c8b2699381a24ccc780"],["/tags/中间件/index.html","9af0d002ceb5962ad548e6258a30899c"],["/tags/二分查找/index.html","fc1240e1b1b57f552a225d68f6d9a2ee"],["/tags/优化类/index.html","e334a1edc359bb85191457cdeaaeeec8"],["/tags/前端/index.html","a2d1c641fdc2e0a9affac19d4079db80"],["/tags/前缀和与差分/index.html","268c04f2218eb103eb3f12298847fd19"],["/tags/动态规划/index.html","b1d1bab8d94b4aa5b3772d749a3d392a"],["/tags/动态规划/page/2/index.html","e8346b8a317c85084c146ad1f2650600"],["/tags/博客搭建/index.html","7707f91a8b77e4d9bae7e94dc888092c"],["/tags/图论/index.html","fd5e2870117c1a1d79bd79fb43a73a78"],["/tags/大数据/index.html","82a40b0f5a4da1975a716dc213fb2119"],["/tags/大数据/page/2/index.html","d63345f7560a232db0a0a4c0c63b4b48"],["/tags/操作系统/index.html","2294a22d65fc94b99e6b2591c3d014d5"],["/tags/数学建模/index.html","7d5598ea31d997cbef2535ebac39a6e7"],["/tags/数据库/index.html","aeccbd3ae5b2d60b0c143689e6130326"],["/tags/数据结构和算法/index.html","745bfa2f54aef43d16bd81048f1ab0a4"],["/tags/数据结构和算法/page/2/index.html","0a7f076866831081722f284ce63f020e"],["/tags/数据结构和算法/page/3/index.html","c5918c814556c81067f8e299641b736c"],["/tags/数据结构和算法/page/4/index.html","ecabb0056d4863bc352b200e280f1254"],["/tags/数组和字符串/index.html","fa0234ae031250e222fb8566b1f073e6"],["/tags/数论/index.html","add3e5cca1ba3922325185ff28b2efee"],["/tags/枚举类/index.html","2d2447d9b543dcdf77cb2fcde504e710"],["/tags/栈和队列/index.html","f99794f9e9e85ae16a3562c5ec3ee6bf"],["/tags/树论/index.html","31f959602ae0e8a2099581926a5dbed1"],["/tags/测试/index.html","d79c5a7521fbe920593daa41be6a6013"],["/tags/环境/index.html","05f6e048289d864fbbafefda1d7a2331"],["/tags/环境变量/index.html","5832d03a3c9dae886b063db6d39fc8b6"],["/tags/绘图/index.html","2afcafad71cf0cd82e5439dde455e9f2"],["/tags/编程工具/index.html","60cdf37b40408c84a99e67c6544ce4c8"],["/tags/编程环境/index.html","8adc0f0889f5ac94859c5b75083330ad"],["/tags/网络编程/index.html","865822513cd3534f60d27f88ed30df15"],["/tags/英语语法/index.html","4111ea805fdf5bdc88ae5ee5a9193bff"],["/tags/计算机操作系统/index.html","63276e67f30694480e9490e9e689594c"],["/tags/论文/index.html","8685f087c88fc28659221764b759ad1e"],["/tags/资源下载/index.html","ce743dfcd02feaf6ed9b8aae6c3a9501"],["/tags/链表/index.html","32376cca2cca0bf87c5241443bc370a3"],["/tags/集合/index.html","8e1a3769b11883bdcd7c5a6f26edf613"],["/tags/集群/index.html","26bfe04ed66ae8b8d3de7c2dff6f265e"]];
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
