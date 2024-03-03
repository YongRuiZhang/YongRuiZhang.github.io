/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e3d48464d95a3fb32c93ca6b759f7024"],["/about/index.html","d5786028fd9adef61cf50cc1dcaa921c"],["/archives/2023/01/index.html","631e0f768b0a65c25556aab3e7b40b68"],["/archives/2023/02/index.html","27fbb8678c54a45798c77ea42491e507"],["/archives/2023/02/page/2/index.html","6ebcd446ff25c15c2428693beba401d0"],["/archives/2023/02/page/3/index.html","9283752f5750e02d4cb6a7c16bb1c78d"],["/archives/2023/03/index.html","f388790c1e0c2e7bdb887511501d23a7"],["/archives/2023/05/index.html","9ff50999605170d149da7afd7f36a948"],["/archives/2023/06/index.html","64234bb570fbf4f9db0c604792ba430e"],["/archives/2023/09/index.html","b7292157c0d4056b2d22fd16df6bde90"],["/archives/2023/11/index.html","d9c005823dcb361fda153d426bf54b2e"],["/archives/2023/12/index.html","24c4d8a446479de30e4c59e4980f7179"],["/archives/2023/index.html","adfe06e81fbdff234b46a913f36e0af4"],["/archives/2023/page/2/index.html","30deb32eb199c8d4561d3aef61730ff4"],["/archives/2023/page/3/index.html","1ab17861734947f0304fbaa7b898cfbe"],["/archives/2023/page/4/index.html","530f0bf367aea119b7736da11d50ce57"],["/archives/2023/page/5/index.html","ec3e73f2f25de4839da5cfe5c1d894fc"],["/archives/2024/02/index.html","28b36d72d76d403ce6b434fcbd94e470"],["/archives/2024/03/index.html","d9cda188c5a62a5b3278480650ac7431"],["/archives/2024/index.html","6b5cac703972147c88bb65c3759fa9ee"],["/archives/index.html","4648ab948e1971e10a9de1c905a501a7"],["/archives/page/2/index.html","cff0957431b8587579ccb8b63230c6da"],["/archives/page/3/index.html","16e06bf80968b47e3831b6361ed7c232"],["/archives/page/4/index.html","909d748619d985ffaefa26eaa5c27825"],["/archives/page/5/index.html","edae102f5e61d77274391a6867486e82"],["/baidu_verify_codeva-qQP2iZOMLX.html","4687187479afaee40e0d8c1d59fb7287"],["/categories/Java/index.html","7d585ae073dca8a7669f43691e119bda"],["/categories/Java/后端/index.html","210bd8e0da532d4babc053babbf44d75"],["/categories/Java/基础/index.html","727d15ff9ebdb17e8304797f0d721fbc"],["/categories/Java/基础/集合/index.html","dbad070ed1dc3cc3e3f881116e9a586f"],["/categories/Python/index.html","16ecf91d58f4c92db65719c4511730e4"],["/categories/Python/编程环境/index.html","f50404ae4a732577e249345873b08d23"],["/categories/R语言/index.html","d2d861348acf5ec3a77a0ad9128970e3"],["/categories/R语言/编程环境/index.html","1a97fd1655d8f943fa59999516170582"],["/categories/iPad/index.html","3fd7d1df36b363ca763813b1d1a842f6"],["/categories/index.html","fc9ff27da595cdfefe214dacb97da741"],["/categories/中间件/index.html","3b1efd4a3c39762bba1c42a473cadf96"],["/categories/前端/Vue/index.html","8f36021cfa3081b66e88998db127ae16"],["/categories/前端/index.html","e555f4066b99f6c034e9d088bc46203d"],["/categories/大数据开发/ElasticSearch/index.html","b333f493f5176749c47460a5b4d9b164"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","630869354c1db3e9cb5ef1fa7f3fc929"],["/categories/大数据开发/HBase/index.html","0a8de1293b78bc59c251dfade855c38a"],["/categories/大数据开发/HBase/学习笔记/index.html","9a3a254724e71823d24357603a63055b"],["/categories/大数据开发/HBase/环境搭建/index.html","26a5754852df1b223f578ac81408a46b"],["/categories/大数据开发/Hadoop/index.html","7e6439c6a2622caeea016aea35f4da74"],["/categories/大数据开发/Hadoop/技术/index.html","680316e733d484724f5e73a8de61f0b5"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fa94990ec8af8760a34f0be47d2004ab"],["/categories/大数据开发/Redis/index.html","8993ca1c7737b0acaa882e055a03039c"],["/categories/大数据开发/Redis/技术/index.html","76894b540775b1a4e9b0bd0e595d411a"],["/categories/大数据开发/Redis/环境搭建/index.html","5cfcae4098f2690b109a459fafa817ab"],["/categories/大数据开发/Spark/index.html","d4531516c30de348b475d2b4760fbb5b"],["/categories/大数据开发/Spark/环境搭建/index.html","706afd8f2a50b48b3feba00c063e18b6"],["/categories/大数据开发/Zookeeper/index.html","03cf7db7e9a988c9835975d2db8d1504"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","80f8d572d7d61acc99934265764681c4"],["/categories/大数据开发/index.html","d70aa214151b58079e498be7f5fa8d44"],["/categories/学校课程/index.html","7a9309c5d5a4f52f380ec3e6dd9317c8"],["/categories/学校课程/计算机操作系统/index.html","145fd4368dd44e67e80e17d4c5f17f5d"],["/categories/操作系统/Linux/index.html","29a77f97b1dcde7dbf4c7c0b0c1469dd"],["/categories/操作系统/Mac/index.html","0c4d8a1b4f6a995c128a4d9d2a52aaa2"],["/categories/操作系统/Windows/index.html","6803e875fb07f0c092562b754625033b"],["/categories/操作系统/index.html","79ef038a6d0b54437675683ac3527114"],["/categories/数学建模/index.html","96d92562a58c2a3fdf47aa59ca3337c0"],["/categories/数学建模/latex/index.html","04c4278c3dc67ead56153ae40daf03aa"],["/categories/数学建模/优化类/index.html","98692588004fd85954ea0f317df04507"],["/categories/数学建模/优化类/现代优化算法/index.html","9066d20d3c78dc50e689e90078db3fbf"],["/categories/数学建模/优化类/规划类/index.html","5b9ea74356a043ff0807896cfe3002e0"],["/categories/数学建模/绘图/index.html","f6172ba05429eefd9134fbcccae33851"],["/categories/数据库/MySQL/index.html","e153e9cb0b4a3a5150a87eaaa5beba84"],["/categories/数据库/index.html","8b8c1c67ea2d47c32dd8e0f94907c0d9"],["/categories/数据结构和算法/index.html","219ecd8b0ddf9011b203b2d78d9423fc"],["/categories/数据结构和算法/page/2/index.html","32c1aba5046b6b63828dc4008cc8685f"],["/categories/数据结构和算法/基本原理/bfs/index.html","2cf46ca8943c5580f34752d51cf72b2a"],["/categories/数据结构和算法/基本原理/dfs/index.html","2097e95c4447a10ebcba05dc79a98390"],["/categories/数据结构和算法/基本原理/index.html","3791218fc4268a8d404dcdaf2d1d1749"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","4564b41c4948bb8088278fd2a7548218"],["/categories/数据结构和算法/基本原理/动态规划/index.html","cd5ec2ce4a12710732a3e05261feb2cb"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","fffd79f17d23b3e472c6fd3d77f15c41"],["/categories/数据结构和算法/基本原理/图论/index.html","af34e1d13e84ec4d6e4ceeeb2fa09180"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","3feaf2da67d28699320fc995c71e75cf"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","41a1e5fad0f5543e6e7ab5eaa77a66e0"],["/categories/数据结构和算法/基本原理/字符串/index.html","9b4207c51c11021cc7b55b277a6fff49"],["/categories/数据结构和算法/基本原理/排序/index.html","4f100d25500aeca62b0d785bdade776c"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","43318ac91dafe4d2c3eb1274d31edce1"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","806458b42f06c179823c37786041b552"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","97cf32b8fe6a241e837197b928ac12f1"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a7555467c5b3d064c47ebbdfeb4771d5"],["/categories/数据结构和算法/基本原理/链表/index.html","5805c78d03c6fd1652bf9e7bdc430db3"],["/categories/数据结构和算法/算法题/index.html","bce6e6644859169b65f3fc93c4e89016"],["/categories/数据结构和算法/算法题/二分查找/index.html","6b96c674bb9fc66bc36f8778125c3db0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ad5a6dc9e0377b60c2e8a3679d8aab0f"],["/categories/数据结构和算法/算法题/动态规划/index.html","fc5eb713b8c4a191a9e62c58de6240e1"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","bfe1a319bcc9fcfa43b09e6168478ed4"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","83212182d5fba4be0d9b27f22ab352cc"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4363b0747adf68abcc947b1647a2cda9"],["/categories/数据结构和算法/算法题/图论/index.html","c20ed25b97bb9a553759090c7edb8d13"],["/categories/数据结构和算法/算法题/图论/树论/index.html","4256c9a5f3cfece2e34b2ccb1b720ac4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","3bba6a24b1dfffc919ddcf9ffa2d5da6"],["/categories/数据结构和算法/算法题/数论/index.html","84d0a15fe1293948d1ca7ee677f9f8c5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","3f80c36d6d396ff820449af91c60bf6c"],["/categories/杂七杂八/index.html","802e239634d2c8f39a72cafc4aedc0fe"],["/categories/杂七杂八/博客搭建/index.html","e9bcaed9418cb9c17920d41713b68e27"],["/categories/编程工具下载/index.html","95a0502f896e44906199cbdd462c25f8"],["/categories/编程环境/index.html","9ff86ae1d2836da5c58bb384bdbb4c74"],["/categories/编程环境/大数据/index.html","f48d764e5a6e72130f4e764772adc113"],["/categories/英语学习/index.html","7c7dd395d1fdac890dc406646d1c3990"],["/categories/英语学习/英语语法/index.html","32df7852a72af26bf2c918b6b05ec05b"],["/comments/index.html","70b243b6435b460d4306a9bf8db5e522"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e4796ca3f4dc940bfef1e425a168c23a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","f17f006948339f71ed717ba684804844"],["/movies/index.html","c4efe30b0f738665e0feb10846ab4859"],["/music/index.html","bcba038c9af70c65e8691702f146d486"],["/page/2/index.html","7b72e7054c99b052f4a1218373d6438c"],["/page/3/index.html","f78068d21396242ad26fffc7a3ee508e"],["/page/4/index.html","6492465c8ad1a7d524f44bc648a67a2b"],["/page/5/index.html","2d7bfbe83cd7b0c2526a7f467d862e79"],["/page/6/index.html","38c6983f4f83529c5ac6aa80a90b2c2d"],["/page/7/index.html","8b48acdd2bcc030ec4a0dabcccc1fdd8"],["/posts/1021360842.html","9449c4bf2067116904e9f1a77dea1898"],["/posts/1120620192.html","0e2655c4bda6e94de5e243dec11fe4a5"],["/posts/1137707673.html","d526e093dbe57461e6f331d303583a07"],["/posts/1141628095.html","b396af38ea9ad95c854f28269bc8483b"],["/posts/1168613674.html","8c4b1089d940611b9873d2d75d2fa816"],["/posts/1219920510.html","275221e0265151493aaa51d28c5fb56a"],["/posts/1222166338.html","9d41313812f95258fce62964eb470f6b"],["/posts/1259097482.html","0b8a2f5c6c655db24c3db25750ff2fba"],["/posts/1271036369.html","bf35cf597d617db17527da5089ef80b5"],["/posts/1312847445.html","e5abff1bfc0f2bf086b9467775115aee"],["/posts/135355774.html","90f611019c500fbb5e285878982d11b2"],["/posts/1375344716.html","84a51de11a3b102bf9fcf0125a4324b1"],["/posts/1388991698.html","e76d9c6f2d70f4ecf3f8a93cfa76baf3"],["/posts/1410315814.html","2d23fc336ed41201136225ceb2c28d15"],["/posts/1452790229.html","cf74da35ac265bb51aab5bff358d65d7"],["/posts/1470079884.html","d1177c23de0dd8a231d79af069f6d5a7"],["/posts/1470079885.html","9d3d7b495bbf363ba86a5b4fc3a23fe1"],["/posts/1470079886.html","632fa49e374b327af69be1658337e478"],["/posts/1470079887.html","5d35297dee3ffe9c5841edc88daf9eb2"],["/posts/1498536549.html","b0afa35c26cc875555afef22fef76e0c"],["/posts/1539568593.html","33751f0998b3e7ff11936bed77aa8665"],["/posts/1547067935.html","c97464d542f1d1ecfc3a78b7894a3a90"],["/posts/1557866301.html","5e512aab73c3e3c38eedb405629db044"],["/posts/1571776361.html","9b4afe291ad996f28f1e9748a06ae08d"],["/posts/1605124548.html","9ae9791c94d14bb4e356437c51556f3f"],["/posts/1633036852.html","9755f5fa8e347adeb86591101b35381b"],["/posts/1667740714.html","921f08e42d637e005b0200bc20ba7b2f"],["/posts/1674202625.html","5a87346bd1fad53dbd6628204d73997d"],["/posts/1765123828.html","0c8a4f6201c6fafc5ebe29bb1434fa72"],["/posts/1767336200.html","142cb6f55c212e9c97d344351491bcb4"],["/posts/1776114197.html","3b12ef33807bf33f68e1d835ee7475c6"],["/posts/1817748743.html","8e612aebc7d7702b266da53240813005"],["/posts/1925125395.html","d1eb86d9e9f2e6d63921f1af4572c068"],["/posts/1966191251.html","29ecea2e3f0ad4693f5f0abb41dfc999"],["/posts/1987617322.html","9a749d941ab245b3b4ce8d5f68290716"],["/posts/1999788039.html","313a7d7fcfb581efad641f715ac78641"],["/posts/2007534187.html","976ad8db263a68377f21704ab776493a"],["/posts/2075104059.html","89a1297b1cc21d39b5cabce9edec5978"],["/posts/2087796737.html","2f27075652a3bf651fef334482e2b5d4"],["/posts/2106547339.html","86c3112dec17a247c46b1f3027863926"],["/posts/2207806286.html","674c89ca3f29ac50fe47bc8062e285bd"],["/posts/2225903441.html","6b9f38fa9df364fe399c1988e0e97abb"],["/posts/2265610284.html","f2533aeb89a9941ce220039a8188d1d6"],["/posts/2281352001.html","8d3fec261db81d8de638ccde195da0f8"],["/posts/2364755265.html","aa78f7f7a811d040fb35a4d1c293933e"],["/posts/2414116852.html","73ab9a6893b2b54afc8614fafb94d7f4"],["/posts/2421785022.html","73ef975f57a1f118c1754b6d3ccf800f"],["/posts/2482902029.html","7d14eeecf4211a0b3764f16142b5050a"],["/posts/2495386210.html","4df252d945f03c18b5a1e4e3bb5d0a7f"],["/posts/2516528882.html","68f962db46cc3e0c01341990df00389f"],["/posts/2522177458.html","37e84491f6e8a128d8574d76e12609f1"],["/posts/2526659543.html","949625c6b4987a3e0dfcc3eea5a3c30a"],["/posts/2529807823.html","ce2d47a3df9f47778f3b15e6632d2b90"],["/posts/2592249117.html","6f9c8737ab2b4e4cfcdaf9a1ab6d9f2f"],["/posts/2596601004.html","0964cfe78627ab31e0d5d2a72d9f8207"],["/posts/2697614349.html","746b0ef2d16a31326f5496b1a4bf3d73"],["/posts/2742438348.html","2ccf4bf4466f86f13940ebc60093b6c0"],["/posts/2768249503.html","5725a7863815b258b598c8402698507d"],["/posts/2864584994.html","3e9f09b61afb07d6a03e11dac957c1ae"],["/posts/2888309600.html","f4dda9c651a356e25891db3aeee826be"],["/posts/2891591958.html","7d4f1d7462ddeae280beb7df4544b68a"],["/posts/2909934084.html","7ed33af0d8492d8625a3a16545c45643"],["/posts/2920256992.html","adfea26923da9e41a94fdd041c28a54c"],["/posts/2959474469.html","e987078ccdb2b802db401f1aa8c108bf"],["/posts/3005926051.html","7f3cbbfa58e693ecf16432b1dee5c652"],["/posts/309775400.html","e223f8d4b79151d5cfc43e72b5283fef"],["/posts/3156194925.html","dabda674dba7aa29cae103f1278259ce"],["/posts/3169224211.html","f7645f31be683a5e82cdef78dcff15cc"],["/posts/3183912587.html","bec0f0688bd8d13e88b59de8023a8874"],["/posts/3213899550.html","e78ef545591ebb4e497d60a3bddc00ca"],["/posts/3259212833.html","2a5560dfd3af8cabaeb77381709b97e1"],["/posts/3265658309.html","cf740347d2a48dcd689714622635715a"],["/posts/3266130344.html","45633b08648ef2cac2db9d19be7e9074"],["/posts/3292663995.html","a2a3a5648febc1038edfe2756fbf498c"],["/posts/3297135020.html","89cf439b97cfa571168c0d07f6f5417f"],["/posts/3306641566.html","f5bdab46e659ef2b8f6eeb0bd06c888a"],["/posts/3312011324.html","bf0a0607f4236f5c7578c12e890b2c10"],["/posts/336911618.html","97f809bc23bcd25f7dd768947934a7a6"],["/posts/3402121571.html","2801240e365f7652b35f84271e308cfa"],["/posts/3405577485.html","99ae103ed01f3e0d34ee586b949f3e76"],["/posts/3498516849.html","e896ff6558135ccddbbab5976c290e12"],["/posts/350679531.html","db1bdf529b1015b5b6ab53bc7cfd05fa"],["/posts/3513711414.html","0501d0c8da34e3d485b69050a0eb67f7"],["/posts/3523095624.html","1207941c43432179d35b17dbbf5d69a0"],["/posts/3546711884.html","7d49785e54c5884dd4e13d61530e3351"],["/posts/362397694.html","256f2c37c68201fc49dcd2898cf91044"],["/posts/3731385230.html","ab87599cc37f56873a4d3ad24ab7b99f"],["/posts/3772089482.html","fbb210ec95aa5d88088bad012d9c6541"],["/posts/386609427.html","16d082874b50e73090e6ebc178cf4076"],["/posts/4044235327.html","3e4ddbe84417568981d3d70367035dd1"],["/posts/4098221856.html","4c083abb007d39c0ea1ff79518f1c527"],["/posts/4115971639.html","98eff4ba855eb7263646b360271de066"],["/posts/4130790367.html","d66e135b77b4bb2234797f9bb6613a71"],["/posts/4131986683.html","163686cae64e4fa119364b6351af4e8e"],["/posts/4177218757.html","5ebf00ed5940e3d340284f882dccf9d9"],["/posts/4192183953.html","ad7eb8016edff94c8af54e2978e4369b"],["/posts/4223662913.html","281e1f496a5421d40e9a7e5d0763a6d0"],["/posts/4261103898.html","0ae9d9a8756afdb31d543961760be752"],["/posts/4286605504.html","ccdadd4732984f889952a619df7c1a44"],["/posts/449089913.html","430d5de0991c8d0b18af0ca325c5db73"],["/posts/469277133.html","878a11069f61c2e5aa53d0205247c56b"],["/posts/469711973.html","c4d55c86ca321595f4e53027a84d2785"],["/posts/482495853.html","82ee4be377236a2ff46a18241919e93e"],["/posts/488247922.html","fcddd67cab96936880f1f360c04c5436"],["/posts/517302816.html","9a4ee33625f88f3d8452b5f8d38fb337"],["/posts/570165348.html","b09e54b5dda4c6f9cfd61b9edf504128"],["/posts/595890772.html","946f11f52ed214c76801245a3cd33f9e"],["/posts/67485572.html","7b4901fe8a74be8213b7c1544412a5dd"],["/posts/694347442.html","1599eb1b123073585e35464bfe03ab00"],["/posts/707384687.html","5d21c4288c620101c0cd8753d59090ee"],["/posts/71180092.html","772bdb482e53d7bd1f7aad3651e2b76e"],["/posts/716459272.html","c6085e5cc927ec45e3f2e4935e10670e"],["/posts/765481613.html","1b9ccd7afdd1e8376ab11f120d66b4b1"],["/posts/778231993.html","726c59c36fecde37fb25ffe8c6eee770"],["/posts/795397410.html","e974fa4c2350d4918ac801618d1ccf00"],["/posts/820223701.html","c40a6696b3136f01b67f4beaf54b98c0"],["/posts/830372185.html","203fc1afff1987c788aee3c6df2c654c"],["/posts/88294277.html","d5be4eb09769f3c394d30a717ff023bb"],["/posts/939963535.html","8ee8104a18ceb5ebf150ff4bf2917580"],["/posts/983786067.html","1db12b51323fd54440067df80f85d480"],["/sw-register.js","75f7b317e88fa3e2ba837e003300fd98"],["/tags/C/index.html","894685f49ba39b21baf1886f82e844fc"],["/tags/C/page/2/index.html","c12d661f19efcf47fa3f69b1f51a13e8"],["/tags/C/page/3/index.html","41fd72b12a8c98090ce94c6c29d05c9e"],["/tags/C/page/4/index.html","24a728ec98a0e4a87c28343f561b1c5e"],["/tags/ETL/index.html","54a4768b91c2559fa66acf0613e65c5d"],["/tags/ElasticSearch/index.html","066222bbc54cade4e8dcb4feb41ba0c3"],["/tags/GUI/index.html","d00fa8dc93eced3d084e7100546b54ed"],["/tags/HBase/index.html","dec688d17540e3e94de99e4c5ef2e379"],["/tags/Hadoop/index.html","507f855d1d44e5f6c2e020d207477a8b"],["/tags/Hadoop/page/2/index.html","52c37c7ef9d35bca7e4b2df4d0e50b3a"],["/tags/Java/index.html","3985b42f7f999b311ea9710fbfeb2284"],["/tags/Java/page/2/index.html","c1face0b903fccaef472a1de9f9db2ff"],["/tags/Java后端/index.html","1ab0b0963f518ba2f3b0eca3eabe1159"],["/tags/Java后端/page/2/index.html","167c0571fefd5785921562978dbf2c0f"],["/tags/Kettle/index.html","73c39a67d7f390c19355deebc9fb88a8"],["/tags/Kibana/index.html","b67cab0a4a4a1bca5e5b9fe38c5c3755"],["/tags/Linux/index.html","1d59a575c50a617a64ad6f490ba55e02"],["/tags/Linux/page/2/index.html","54ed258ec68c1f87a444ba68a61700cd"],["/tags/Linux/page/3/index.html","29644d6d6661b12498967d3f8b48d8f0"],["/tags/Mac/index.html","97a1e3c79f71a920430b32f1cda47bfe"],["/tags/Mac/page/2/index.html","aaeda01136c920128fbe69a451ffe2d4"],["/tags/Maven/index.html","f933303069829ad6f0335c472ba32984"],["/tags/MySQL/index.html","2e4b78bb2bccc707dee547d8a8071764"],["/tags/Python/index.html","2888078ac90436248e4c8b4557b9c3b8"],["/tags/Redis/index.html","a5f350f4ac95b5e4b9d2967464de8d11"],["/tags/R语言/index.html","5de02a548f4dab2d3dc70f43ac769d60"],["/tags/Spark/index.html","865d2b995af06d2f8769b9e2e86af2ce"],["/tags/Ubuntu/index.html","ab3e6da47b0fca16722f435a9fea670a"],["/tags/Vue/index.html","89e56f15dad23ef137fc1948a882b554"],["/tags/Windows/index.html","7c5428b3989e02861d25ff77449ce2d3"],["/tags/ZooKeeper/index.html","d033583b2188a79f3e48fd5a117d046d"],["/tags/bfs/index.html","6d5f4162c9df93a2547739f92a36a1b4"],["/tags/dfs/index.html","fe7c87dbfc799e7b726f51a236a1da5d"],["/tags/folium/index.html","28bb7e3f8ab9909329c9eb2de29e75f2"],["/tags/git/index.html","d8b49e260ce57b57c9ebbda0721ff674"],["/tags/iPad找电子书/index.html","62beaab1a4c8fa65e0e9ef639a95cc4b"],["/tags/index.html","deb852bd84ac9585a8bf0f41fc8660c2"],["/tags/latex/index.html","35fceb618a2c4113ca1807047f17c693"],["/tags/中间件/index.html","e1141116172fa7a4e1d23ae85e33e015"],["/tags/二分查找/index.html","ecec96f0c1d4a886c38615376c71565d"],["/tags/优化类/index.html","d18fcefab854e7a814c8a176b0eedb59"],["/tags/前端/index.html","f77e2eca6a856a3397cab8ac6e334a01"],["/tags/前缀和与差分/index.html","07eb4359af010d68c3f7ebce84c3d51c"],["/tags/动态规划/index.html","8158b85e14fbfb5930a05d3932b9c725"],["/tags/动态规划/page/2/index.html","c9a71ba79d6e6efca889bcf09f62f8a7"],["/tags/博客搭建/index.html","88cd8ebece04a38b2c4cc607ab556e3e"],["/tags/图论/index.html","ab89ed32bd9010d0e0fcd215b652f168"],["/tags/图论/page/2/index.html","6feeb31470b20c031bb4c80d875e2669"],["/tags/大数据/index.html","4c20f09fd562951a4bda1c7641adc1d8"],["/tags/大数据/page/2/index.html","8954e273a3120e775e4eca39c830f6ad"],["/tags/宽度优先搜索算法/index.html","282081dcee5fdd0e67bb83a14d2d16ca"],["/tags/排序/index.html","cc9f84cff78c379faea0e41fd1167489"],["/tags/操作系统/index.html","151b8d35c007c52574746ccb57215425"],["/tags/数学建模/index.html","1e438d4a92af62e55b287b4bb6bbca43"],["/tags/数据库/index.html","46a3911f440bd368752fa92eef463a84"],["/tags/数据结构和算法/index.html","0209d9db73e718c85bce1134c2a2c15f"],["/tags/数据结构和算法/page/2/index.html","8b5c99e2c7e6d1161a0ba74f81dca93e"],["/tags/数据结构和算法/page/3/index.html","904ba044bb4a016a90dcfea9f48b3a17"],["/tags/数据结构和算法/page/4/index.html","744695cced51532cddb7321c7327f08e"],["/tags/数据结构和算法/page/5/index.html","bfd3570649a8e174c1a546974b59ec5c"],["/tags/数组和字符串/index.html","e10e1180b8327f08efac97aac874c144"],["/tags/数论/index.html","24a45bc80d00029de7c53c64ab426175"],["/tags/枚举类/index.html","d8298294665d3c7ffaf7b4faa3c18f64"],["/tags/栈和队列/index.html","eca50ca1a4c968db912360e3c75e1579"],["/tags/树论/index.html","7e28b86f337731beaae1b6c2253af7e6"],["/tags/测试/index.html","ab6e032daf3757b2df8ac22b155b30d7"],["/tags/深度优先搜索算法/index.html","bf9ecf6e2b91387965ecf9fea8fa67c4"],["/tags/环境/index.html","556a0f371b0076a9c41c8e4983d1eccf"],["/tags/环境变量/index.html","e0afadfaccd6a8538324b17918ee4f7b"],["/tags/绘图/index.html","f4bf1250c5b5b7a474d4ce5c44b6970c"],["/tags/编程工具/index.html","2e64f70de94b04cb316a32a9b5b14b7e"],["/tags/编程环境/index.html","884a3956a5f25bb2db2db70d9244960c"],["/tags/网络编程/index.html","0c9f04768300f6e2bcd1bb15e9d37fc3"],["/tags/英语语法/index.html","7db95c037a98fb94bc2aef84d0cadcfb"],["/tags/计算机操作系统/index.html","efbc40763ab8f8ab7dd2ed3976b40d35"],["/tags/论文/index.html","c7e8ebe1dcc1f97a59601316e9dc4219"],["/tags/资源下载/index.html","143547b51928e200c689f62e9f835f70"],["/tags/链表/index.html","e53ae0ef0a73a1ce8d1874919769fbfb"],["/tags/集合/index.html","a85ba9c7840da200648523272ea38a89"],["/tags/集群/index.html","a18cd8c88752058b99d9d814f0eef128"]];
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
