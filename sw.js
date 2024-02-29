/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","35b57380d315eed41d5eddcdc11ce884"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","da023e4e48c44d23437c4ce291cc69f6"],["/archives/2023/02/index.html","8c8ae05546425ec8c668af3ea5d8a100"],["/archives/2023/02/page/2/index.html","4262196678309433ef357b0a63dd060a"],["/archives/2023/02/page/3/index.html","7aa2be07f2335a001b9b58c22eeacc78"],["/archives/2023/03/index.html","f36ae3367552d357cafd9a7c78b9f96b"],["/archives/2023/05/index.html","1817e9389409e1938bf4b1bb45bed1b4"],["/archives/2023/06/index.html","707830e5869b04549269f9202d5f5ac3"],["/archives/2023/09/index.html","a5989694d05ab383ab082b30e2620625"],["/archives/2023/11/index.html","6453053d3dc3988ae3393d594c2b5af3"],["/archives/2023/12/index.html","8333093b576f8e26492c91fc5bb97493"],["/archives/2023/index.html","4c1305fd51bdfabf5077ab9e09b7e2c8"],["/archives/2023/page/2/index.html","8f076e152eb265377fc12845fe5a8664"],["/archives/2023/page/3/index.html","abdbb3069fe2dce0219c648921577a0e"],["/archives/2023/page/4/index.html","f3f1b055cce31bae9f5cdc6f475cedea"],["/archives/2023/page/5/index.html","5221a1ad0c45c7377a7f70f3f4592c6d"],["/archives/2024/02/index.html","726899e58d552adfd13db9c8cc2186bc"],["/archives/2024/index.html","8a748f7daa4358c5241775e2546c6bac"],["/archives/index.html","de746b30d9f4a52f2f03e4f8311e5be5"],["/archives/page/2/index.html","1c10e2e320b8c058499459b796d990c0"],["/archives/page/3/index.html","081f20f826b21cd5d29596a6b6f1f0aa"],["/archives/page/4/index.html","45da2ce0bec0d2f436c70b3c6a26c03a"],["/archives/page/5/index.html","0041a064dba9992ae2256a4cf703bc26"],["/baidu_verify_codeva-qQP2iZOMLX.html","0aa47966ada7a324faf3232cf5b4fc99"],["/categories/Java/index.html","6ea5f93047e1063c386420957dbaf3c4"],["/categories/Java/后端/index.html","04deafaaf354691a8f0cc3940d8310a0"],["/categories/Java/基础/index.html","bf1748284ed128e73c3c98c7e03e2898"],["/categories/Java/基础/集合/index.html","04d7dea5acc08d36926028def4122ae6"],["/categories/Python/index.html","297cc4a1da6e2dedad6bf159fa876fca"],["/categories/Python/编程环境/index.html","950615743c0e9ddce7bf37fd94daad24"],["/categories/R语言/index.html","5dc0fa4d5cd03ee5b5c0b17a0e4c9c3a"],["/categories/R语言/编程环境/index.html","46b5f85f061b82548ece60e948759cf2"],["/categories/iPad/index.html","0dfbe5ee405cbe74e3df211d9a315663"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","e8b54814874e0a24bdbf1a3c4bde2767"],["/categories/前端/Vue/index.html","2410a618cd97fafc65028709feb9738f"],["/categories/前端/index.html","63bfe47150cba2c754a467f7f70e8094"],["/categories/大数据开发/ElasticSearch/index.html","a3feabc352fe9df0f726ba89de06b1e2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","ce921d742bf833ef96f27a50301558c6"],["/categories/大数据开发/HBase/index.html","12c114dec5a27f2c241d6f84facaf7c1"],["/categories/大数据开发/HBase/学习笔记/index.html","f3ce8f8a2a8b3b693ce7a8e4c10e1ce0"],["/categories/大数据开发/HBase/环境搭建/index.html","42f34103ee82c47f42e3b3582148dfc4"],["/categories/大数据开发/Hadoop/index.html","37250f84eac1e902824ccae78af72076"],["/categories/大数据开发/Hadoop/技术/index.html","a6f8a38ac79e82f8c2d5b449aef7ee14"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e4d2de3f7dae98fd544f89546e616500"],["/categories/大数据开发/Redis/index.html","981f47f3c7aba6e05bdfe93509cc6c25"],["/categories/大数据开发/Redis/技术/index.html","f41581fcf40339791a3ab40a7a51fd8b"],["/categories/大数据开发/Redis/环境搭建/index.html","fc7da517232dbe9fe3be1435def5a309"],["/categories/大数据开发/Spark/index.html","ba83d50ff1bf59a446b2af1490808276"],["/categories/大数据开发/Spark/环境搭建/index.html","e2bb79c0bf5416a320d019a4a163688e"],["/categories/大数据开发/Zookeeper/index.html","00d9d423a613071b8aeac2abdf0ab5ac"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","6528b88eab09bcfbbe243c5d1b985afe"],["/categories/大数据开发/index.html","0d37999140a74a9fdd8bf5b1f19136ee"],["/categories/学校课程/index.html","99b8cb5ce9d6ba6986f869e577424ba3"],["/categories/学校课程/计算机操作系统/index.html","977eeedeaf7eb5fd65dd58c0e5963d89"],["/categories/操作系统/Linux/index.html","207b0544282125983fb617856fbe174c"],["/categories/操作系统/Mac/index.html","95d26595a7a4dbcc5ef5664f4a29972f"],["/categories/操作系统/Windows/index.html","c696a4b02bcc9fab7cf5c04f1a3610b1"],["/categories/操作系统/index.html","3080724af2f0a5861f3f4a5b70b69a19"],["/categories/数学建模/index.html","496f496247f481aa8bec1af1a29094b5"],["/categories/数学建模/latex/index.html","6cd9b3dade0d3140adcbfdb7ea3bbce3"],["/categories/数学建模/优化类/index.html","b6ea562b3f1321f564aeebf233ddc6e5"],["/categories/数学建模/优化类/现代优化算法/index.html","b627d8f338764391152723be3217c432"],["/categories/数学建模/优化类/规划类/index.html","360b1f7df6e3a56a9377dc77c6c46bb3"],["/categories/数学建模/绘图/index.html","a92f3ffd421dc258ba12b28d404cc7ef"],["/categories/数据库/MySQL/index.html","3985ef2a56163f7f32ce649422e83cb4"],["/categories/数据库/index.html","9bdd20912dbf15209019ef26f68c57a3"],["/categories/数据结构和算法/index.html","999c90b66a9f31476440a2dc3da3b78b"],["/categories/数据结构和算法/page/2/index.html","6213b729fa5e3b33b75aee93dab09816"],["/categories/数据结构和算法/基本原理/bfs/index.html","e002b5cbcb6e5cc9be10516364d9fd78"],["/categories/数据结构和算法/基本原理/dfs/index.html","bb87d04827fd4bf3c492c67d513e97c1"],["/categories/数据结构和算法/基本原理/index.html","ffddd30e0ad1f47ab8b6ff9bab276a10"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f10438f67009a95644ca2de0da100a11"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ddba66ec7e63c5b4fac51cbf394b79a8"],["/categories/数据结构和算法/基本原理/图论/index.html","164da2ba471e5186808c0f9e5e753e28"],["/categories/数据结构和算法/基本原理/字符串/index.html","6ea79bd776ad7434ba8c4a45a4af2735"],["/categories/数据结构和算法/基本原理/排序/index.html","1d946ff1eec06d1c1b150b56a318c996"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","45f242d8c8e82a13b305cd73aa43491c"],["/categories/数据结构和算法/基本原理/数论/index.html","de000259bd3f9e3c9b7a4f13bfadd279"],["/categories/数据结构和算法/基本原理/树论/index.html","4113de0c646a2fa99d9f13604e5f6101"],["/categories/数据结构和算法/基本原理/链表/index.html","e55dabb243185d094ea34c621daee0d6"],["/categories/数据结构和算法/算法题/index.html","7f4140904acaf5263a24729cf241ebb1"],["/categories/数据结构和算法/算法题/二分查找/index.html","18ec2765b62459ba0a285926aabc30e1"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","bd64a004de504356b54918fb103ba43c"],["/categories/数据结构和算法/算法题/动态规划/index.html","6d2140ce6a1ddd7fd18f7e83aaa096b8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","72c94c8d801d38346763b25046f9178b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e04d98cb6ae4cb7d66786da6d7828beb"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","6e470273bff1d2e50d792577c94167cd"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","644b1768681c50bb3659324224badbbc"],["/categories/数据结构和算法/算法题/数论/index.html","25543e167f24d2a05ee5db430bf6137a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","7f07569a0261f309c639c101729ef9a0"],["/categories/数据结构和算法/算法题/树论/index.html","bff768087b1493862e970f77b5d0c19e"],["/categories/杂七杂八/index.html","9aee7f25bc52825195eea741c9139105"],["/categories/杂七杂八/博客搭建/index.html","ddcb74d3268b27584e6e136b97d490dd"],["/categories/编程工具下载/index.html","aec2de0a86993c78865c7503aada6c66"],["/categories/编程环境/index.html","9c7639c03a84cd5e8ee419bb65531e48"],["/categories/编程环境/大数据/index.html","071528d1ec9ceb0dc6b7d9d6a1946fc0"],["/categories/英语学习/index.html","31437d9aa819161a3cbd4bab03a136e9"],["/categories/英语学习/英语语法/index.html","573b6a474e931cfeaf57fb4c4622a3ef"],["/comments/index.html","3c1d5652f12ca1349b9f9ef2be01f12c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","31b66a3d6f8523e6de6e51d12b9d5af8"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","c2d5b56232ca62222fd7366b225459cc"],["/movies/index.html","09aa7723631d14b8deb94f5c4052083d"],["/music/index.html","ce3e52272da69c6b5f2683260024021d"],["/page/2/index.html","e03bb1bad7bc85d37c4a71ca96d1d398"],["/page/3/index.html","27b8c83ca08c6b1e40630d2f3994e2e3"],["/page/4/index.html","701bbd45b85b31e5f3c8f91cd9392140"],["/page/5/index.html","7e88e1facd588b4f4ce5d74f94de1c9d"],["/page/6/index.html","e7160d0cab095188bec42fac5996bc01"],["/page/7/index.html","fd05d1c67f9c39aaaed1a9c86f9565e0"],["/posts/1021360842.html","77cde43f1c9e02731be0587dd493d376"],["/posts/1120620192.html","cc57de00a208787de912a8cc00deff9d"],["/posts/1137707673.html","dc46f216c38dbe999197544d61d83d77"],["/posts/1141628095.html","5dcab92dbb049f75b8fbfc185857de68"],["/posts/1168613674.html","e4c387880d03ff80ae41ca761d04e220"],["/posts/1219920510.html","214f56646d2bbc3e36c0837121b69679"],["/posts/1222166338.html","13572c545270fdc76a26284ad44e2470"],["/posts/1259097482.html","3f96f4391a890291b8b6404139422d22"],["/posts/1271036369.html","e2b5cced90722a48c5fac5afb2ebb491"],["/posts/1312847445.html","c450ecb27960d1a50542970afdd328c2"],["/posts/135355774.html","56194b8760852a44e92a77f69a4c2eef"],["/posts/1375344716.html","f5804cbedd3353db7ed2862bac662aa3"],["/posts/1388991698.html","b6976f7c3ce5375189661b036d115587"],["/posts/1410315814.html","bacc56ed4a410a85df1caaa8a158b4c3"],["/posts/1452790229.html","636b634b9ba3de14f83c2def23d3226c"],["/posts/1470079884.html","a3273cbe9aba28f60dd5364a1d80a793"],["/posts/1470079885.html","1a296b27713cb0bb7fb070c6bb03ab4c"],["/posts/1470079886.html","5a94e20e9520a466eef5074e767d75b2"],["/posts/1470079887.html","afab5519cd368e7b1af2274295f8d9b1"],["/posts/1498536549.html","46668c88ca87abb65f0706b3532a67a8"],["/posts/1539568593.html","00148abebcf6578027c8ea3eac0c47a5"],["/posts/1547067935.html","05815918066346bc2b1de08b4efb7d2a"],["/posts/1557866301.html","65eaf2e657ac550bade425465e478ff7"],["/posts/1571776361.html","a737e5e0c38fdcb7502506ad53c69139"],["/posts/1605124548.html","2ac5e0de3e36dbfe5cc8cf1a1e9c1180"],["/posts/1633036852.html","2dceb62b0f89af17e19fc5296776e417"],["/posts/1667740714.html","95743bd9caf0cac356ba4a29014f18b4"],["/posts/1674202625.html","89950d850479436f256043634087e4cd"],["/posts/1765123828.html","950cf7e0dd77ad2a2220e00c79714d5a"],["/posts/1767336200.html","a1062d3b222c4f248de1fe38b980b3f5"],["/posts/1776114197.html","6cea81611eb5fa320cd83b79ed470b01"],["/posts/1817748743.html","2c0820e924eb730721d082d7d6688786"],["/posts/1925125395.html","1bc98ca861bdc8c87818133212dc6f35"],["/posts/1966191251.html","64c32439601fa5c3695b9c09e7bfa240"],["/posts/1987617322.html","ed646be01118224f32173c87a9f74388"],["/posts/1999788039.html","0ee8e1295b0cc11c66234f10d573ff84"],["/posts/2075104059.html","54427ce440530a36d6fee4611960fdea"],["/posts/2087796737.html","9389369f75f08e5b0997ce7ed6b664ed"],["/posts/2106547339.html","a8854bd343d4348d4bdc8848aa62425c"],["/posts/2207806286.html","85f96e61580e572f597a573e04f9582f"],["/posts/2225903441.html","70225aa7f10ce7176960badea5d5cd81"],["/posts/2265610284.html","9b1c7d90b615b622109a7df873d1f2df"],["/posts/2281352001.html","e4a0939ff48e24131bd93289e6a0df99"],["/posts/2364755265.html","15edbdc7baeba464db6d255a95d3512e"],["/posts/2414116852.html","ee890550da1e84ab410d70ce1293d793"],["/posts/2421785022.html","ea462ff02eedbd736b46bbf35e02c1be"],["/posts/2482902029.html","cbc13d3872d5957704c4177bee65bada"],["/posts/2495386210.html","e64dc5adc735e606095915388b9a33fb"],["/posts/2516528882.html","3d6a43942335753ef1f4302bdaedaa3d"],["/posts/2522177458.html","4178a6d21be3ddc40230691e5685187f"],["/posts/2526659543.html","87061f02e47fa08a43da4bc73cb19102"],["/posts/2529807823.html","8bb677768d35652d995190791f027910"],["/posts/2596601004.html","cd8feba9397ad74764b0595ec2867116"],["/posts/2697614349.html","98be6745c3a623986d1e39b37561bebb"],["/posts/2742438348.html","2e86aed83abbdcf9e3996094279458fc"],["/posts/2768249503.html","59d14acb3a517d40f493c2c380fcd66c"],["/posts/2864584994.html","299e8a14e8a68f7a8dfa33e3f5de872a"],["/posts/2888309600.html","0e8d1b55135d02e8ba5f927495a9003b"],["/posts/2891591958.html","dbef62aeff65b818d0646ccc7ce7bade"],["/posts/2909934084.html","b4cd37e08355534cc68824d05663ddbd"],["/posts/2920256992.html","e84b3ce1006c2f0cd174b871e707ceca"],["/posts/2959474469.html","20a641cadbd6949c14f9db0c6cc41c41"],["/posts/3005926051.html","42a13ac2dd75fab33574d1bc966f4a53"],["/posts/309775400.html","fd928468026331f41a7c8e2679b3bd2e"],["/posts/3156194925.html","ada5fb6dcb31c29bea8c5991df18d6a1"],["/posts/3169224211.html","3802598e6e3f13cc3d65fd61d3509f6d"],["/posts/3213899550.html","f7f5309361c29ebacd4b9622347ac26e"],["/posts/3259212833.html","858176d5b9eb210bf3b4de00bd91f8ed"],["/posts/3265658309.html","1dbb5d222cfdcd3fc0e86370c659722e"],["/posts/3266130344.html","aee61cbd0d56415e5670da6714b55283"],["/posts/3292663995.html","c27ba57ab836f6654848688facb1998f"],["/posts/3297135020.html","4233861fe273df2a79b00725dbf37ac7"],["/posts/3306641566.html","0a3eb8eb1841f2ad9690a238649a218e"],["/posts/3312011324.html","7cb72b78f52f5137d52f2777fb21c6e9"],["/posts/336911618.html","0f8b8aacc4ccc2aee53ab707aba35820"],["/posts/3402121571.html","bf5525c448418983da0eb05203904317"],["/posts/3405577485.html","e7624ec1f489001709ddbf5bc5800728"],["/posts/3498516849.html","70de118727358af7137bd5825dceed84"],["/posts/350679531.html","75f523d98c4a877dc88266a29f2752a0"],["/posts/3513711414.html","be628a8d389759fa7d7e24bf8d88c4a2"],["/posts/3523095624.html","2fa44d00004a9963f959b0c22e4748e8"],["/posts/3546711884.html","abbc231bdbc57bb24b58768e12609cbb"],["/posts/362397694.html","35af4eec22f4f73368597c6583b06342"],["/posts/3731385230.html","3289737e2826f85b98e270467318b750"],["/posts/3772089482.html","1bf8a5d9da560d851484f0ebcc461dee"],["/posts/386609427.html","2d2afd3f1862956ff9b926670e241b79"],["/posts/4044235327.html","770c97a574039d38c19114f4d692a672"],["/posts/4115971639.html","844b7487a4fd04af8aea047d6afcadcd"],["/posts/4130790367.html","a6e927d2a8bbce4e2271cadc38c44662"],["/posts/4131986683.html","f1cff5a14e334ec7b7e1d87ef34f355e"],["/posts/4177218757.html","f6c3ed8379350a98929a1aa5041a4676"],["/posts/4192183953.html","f8f44ccf736c100fa9ee11f81bb2d021"],["/posts/4223662913.html","eeb98b4dad8c7ff688a47ca3b41c2e63"],["/posts/4261103898.html","9e65e49e999d2d58a5b933fb46edeec4"],["/posts/4286605504.html","11050d31bf75526eff7a4e7f1ed15e9b"],["/posts/449089913.html","8fa92f57935d50fa93dfb0e1db32ac36"],["/posts/469711973.html","0d4a965d2f9c7a42c75f57957558e461"],["/posts/482495853.html","3841f49f1f0cc3e68514f09ee40498e6"],["/posts/488247922.html","754d5aa8c6f062aa0ceebf593448d551"],["/posts/517302816.html","8bc305e2299fbbbc4ea9252af6691510"],["/posts/570165348.html","d30fdb5d59af21bd7a2fbe00c479bae8"],["/posts/595890772.html","e3670d58049b65e52a0828f2982e5480"],["/posts/67485572.html","93395eecfd7fd46c28092ff119209778"],["/posts/694347442.html","d52ae4ded046049a15484250c7e75b5f"],["/posts/707384687.html","e54b88494a019343d1aa667fbfa70a70"],["/posts/71180092.html","8d23ca29bb4d55460ba0ac695ee03ade"],["/posts/716459272.html","0c5a6c9238784982816ebf592a810801"],["/posts/765481613.html","e5410f4383e4e782725dba27cb798720"],["/posts/778231993.html","28014767bde905bda73d4bf58e921170"],["/posts/795397410.html","bf895e9cb1a356669c38de74fab45ebe"],["/posts/820223701.html","4b65be1b1096c2275eb1072f94f9c764"],["/posts/830372185.html","fdec88066427f2b8b5f396df67eea2d8"],["/posts/88294277.html","202882c9208ecac94f23437a84e5902f"],["/posts/939963535.html","92bdc695c6ad883a09265eca6646333f"],["/posts/983786067.html","d90239eede4e06ccc13649c8a17aa7c4"],["/sw-register.js","ff6a0938cfdc7df768e322d3b7788072"],["/tags/C/index.html","2c21ca7348a8f534905e1eb6238f987e"],["/tags/C/page/2/index.html","47ec221f6fd26912381bdf86202be69e"],["/tags/C/page/3/index.html","91819f2de334a4552769f9f74465679f"],["/tags/C/page/4/index.html","74c31570414dd567edba4e70434f9163"],["/tags/ETL/index.html","1ccc43f827e387366de0bd5ec927277f"],["/tags/ElasticSearch/index.html","622105b88772e8900cce1096e4cbafe7"],["/tags/GUI/index.html","16911f9dd63236b66305bd3dd3a43bb5"],["/tags/HBase/index.html","70fe7ab505caee4e66b1a44272e75c55"],["/tags/Hadoop/index.html","6c2f0271d022b2e90a54e12ffc715fe5"],["/tags/Hadoop/page/2/index.html","72644f4b33159bbe9dd76a9df3576df5"],["/tags/Java/index.html","1a437872d8496b37fd673c94f1a9bacf"],["/tags/Java后端/index.html","652e01259dbc2c8239e5c8858fbeaca5"],["/tags/Java后端/page/2/index.html","4a2e0cad90fc3cc8cdb4701552252bfc"],["/tags/Java基础/index.html","f5d8157790124fb0df5663bd3dbfc7bb"],["/tags/Java基础/page/2/index.html","415f03e7ed952b8dea334fe101f8df53"],["/tags/Kettle/index.html","92fea48cdda66a4edb46895c47a11e8b"],["/tags/Kibana/index.html","7d2058c01097100df340059f7f5389e1"],["/tags/Linux/index.html","5d037e6f00e8275bf7f5c7075c56f5b9"],["/tags/Linux/page/2/index.html","b451540170844336e9b572fc77a794ea"],["/tags/Linux/page/3/index.html","2f530cca2a02d78a0986b3648dffeacd"],["/tags/Mac/index.html","856b8c594f9e85d0d9537787aa46c27f"],["/tags/Mac/page/2/index.html","84c6b9f6054aa1b98cc35183a1c30cde"],["/tags/Maven/index.html","c6fcbbb817088376e832a4c04569a0d9"],["/tags/MySQL/index.html","258cfcacdff16bda34610be7d87c6511"],["/tags/Python/index.html","fed3d1f1058a33832a5d09e473f61e5e"],["/tags/Redis/index.html","f805a8efd9d9476da09740bc9061d030"],["/tags/R语言/index.html","d17966e288e7851b6ba079200f37090e"],["/tags/Spark/index.html","f8ce5ddb3c74b27e4fce85a2788aeaee"],["/tags/Ubuntu/index.html","4c373b5d1547a83346f01f0cda3e0e1e"],["/tags/Vue/index.html","06182897a59dd5d56858307d551f6820"],["/tags/Windows/index.html","d57ba4e35b8f87708b08475bbde1f741"],["/tags/ZooKeeper/index.html","81e49195550e105b42624cc146c45fcd"],["/tags/bfs/index.html","7917edb7bd07b06bb79f9ecbae701680"],["/tags/dfs/index.html","50275b51b74017d4ba2e1d662c69f588"],["/tags/folium/index.html","bc045f2fd8fd8f6d6e6dde01f7025b28"],["/tags/git/index.html","bea867aac8f3a5e17eda28e20164f7ce"],["/tags/iPad找电子书/index.html","67d6b9b884596dd431b53a76bf25a35c"],["/tags/index.html","9679f46a4179fe7c26ec920d7b12aac7"],["/tags/latex/index.html","3fb1d95d0a63f93f4f3fca067e2a3251"],["/tags/中间件/index.html","014ab0bc639c0964caf76afda4f531c5"],["/tags/二分查找/index.html","9aa62cbd4fb1ddd61ae66a9f5cf326c7"],["/tags/优化类/index.html","1a947f10519d9e1842d304ceca821d2e"],["/tags/前端/index.html","a47e277aa350cb6f7ac549e8bef36561"],["/tags/前缀和与差分/index.html","c1cad5a37cd6a84f4aa8e73814ce9b38"],["/tags/动态规划/index.html","a86248e56d700099f98ce315168ca8fc"],["/tags/动态规划/page/2/index.html","d0f92247e9b406b32ade56fefb7cabf0"],["/tags/博客搭建/index.html","ccd704ed5a190c81c98235ecbaaf9843"],["/tags/图论/index.html","20c38dc53c27ded522b92a23b15e87e0"],["/tags/大数据/index.html","557dcb92496ac8eb13d3f30b50b1cdb8"],["/tags/大数据/page/2/index.html","1183cb47ffb14d1849f9860c9e4b10c6"],["/tags/排序/index.html","d2337aea29c1ae61d3adb9d145577ece"],["/tags/操作系统/index.html","9ff1c04df9aa009a91433941ff630ae5"],["/tags/数学建模/index.html","e286baba49af1c53407ae9b73a51083a"],["/tags/数据库/index.html","dafeace29a184d63817f5ce462bbe465"],["/tags/数据结构和算法/index.html","a65dee0207811878fd5d466dcc52afb1"],["/tags/数据结构和算法/page/2/index.html","f9cf37b62a5782dd21c60952e2568503"],["/tags/数据结构和算法/page/3/index.html","e92592f04da6b94e6d0492a02b60fe71"],["/tags/数据结构和算法/page/4/index.html","fecdccca3526c850215f86226340f56f"],["/tags/数据结构和算法/page/5/index.html","cc81d63484779acce71c5a637c5d2e14"],["/tags/数组和字符串/index.html","555bc449f8abf2f17b5ebe3c499f62a8"],["/tags/数论/index.html","9307a59b477e487583be4726c2de0241"],["/tags/枚举类/index.html","d417b67305338ba6bc6d53f724c01a8e"],["/tags/栈和队列/index.html","41eda0920d2367de615a60761c627da6"],["/tags/树论/index.html","9bef81119d9d197ee7b7e73353f80beb"],["/tags/测试/index.html","22ef441626234ccca14acf90f5aa5c64"],["/tags/环境/index.html","36ff7b5c2471ceecc6ca4655b05a7aae"],["/tags/环境变量/index.html","797a642b8ab91c82e84d8f4d5ff4b437"],["/tags/绘图/index.html","da09eb4afa6cc0ce313629667a4a1645"],["/tags/编程工具/index.html","0ad257af170041ce93671aeafa11716f"],["/tags/编程环境/index.html","b259fa51baa1ad12afad4bdbff66c22e"],["/tags/网络编程/index.html","6a93f3e4678a58c0912dc16f55db48c8"],["/tags/英语语法/index.html","45464abf2c816d906dced4285cd4c9fc"],["/tags/计算机操作系统/index.html","85e3d4e2c7613970fb8ccd86f5f93efd"],["/tags/论文/index.html","7418a5edd469fef0759cde180f581c19"],["/tags/资源下载/index.html","0179af99994b7e8cc3ebe7d23d2a0b28"],["/tags/链表/index.html","bf7f7903e4b0252632c55f0b95e0b5ec"],["/tags/集合/index.html","adb0c714f4a3f13d5c5aeccd47674de7"],["/tags/集群/index.html","803bad172def47f0fc6c36e4e6aaf591"]];
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
