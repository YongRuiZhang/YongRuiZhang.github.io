/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","94d0753975b82ad9fa3b1ebcb094e8eb"],["/about/index.html","30a13bdcc2291731ee6947dd68ac76fd"],["/archives/2023/01/index.html","c7b0d7231d21fca79b0ef1ea2b424dc0"],["/archives/2023/02/index.html","89d103b6510c8a33e6fbc9e167b9dbe4"],["/archives/2023/02/page/2/index.html","d782672b109427db2d98906fbe29ace6"],["/archives/2023/03/index.html","7a838968ead29586148dad59e255e223"],["/archives/2023/05/index.html","c7ea6705ef54f314afeb56fac73f7d1b"],["/archives/2023/06/index.html","c9bdd4169ef1b27e3cd0624d7c9119d8"],["/archives/2023/09/index.html","e0bc91fde6353ca0a228f315866b3e6f"],["/archives/2023/11/index.html","1d8740979bd3a56c59c98781e608a7df"],["/archives/2023/12/index.html","4e2b86c5e00b8762d4a9384111fd9937"],["/archives/2023/index.html","113c53d627998ce6f0941014c989f9d5"],["/archives/2023/page/2/index.html","0bce72e53e42178b6b8d99938165b0f0"],["/archives/2023/page/3/index.html","0bcf48e7eb2848c570bfd7531db44920"],["/archives/2023/page/4/index.html","2c57b77e334a219805259548ed0455e6"],["/archives/2024/02/index.html","c7c1cd35bb2e12170a608e37e15adb60"],["/archives/2024/index.html","42f5a9c852a5bcffcccd7358c1406370"],["/archives/index.html","9be2bea2512d0fb966a36b25fa4b3bd2"],["/archives/page/2/index.html","a56da79d56916156dc0d2febdc40496d"],["/archives/page/3/index.html","9f376323adba610e1d4e3bc31adfc11e"],["/archives/page/4/index.html","e5f5726d83fc3648c66008905718f6f5"],["/baidu_verify_codeva-qQP2iZOMLX.html","bc83df9e5e62585ce74a360e38bf2cb4"],["/categories/Java/index.html","ef0821c131b52418ada08e2561a57188"],["/categories/Java/后端/index.html","e8fb11e854d25c3f890f97577b9baad8"],["/categories/Java/基础/index.html","5ed9906fcd9a862aadd9fa1c8b3fc977"],["/categories/Java/基础/集合/index.html","803d2a23a33854b7b7313dcf2964f6c3"],["/categories/Python/index.html","382dd84071986eac58bb33582882cccf"],["/categories/Python/编程环境/index.html","33fe47486b1fb2471371850372ddfe2e"],["/categories/R语言/index.html","7041dab946ec44fe49c2c0972451e9db"],["/categories/R语言/编程环境/index.html","be886e69b4820fe403a2892fadde7b87"],["/categories/index.html","63835c7e5e6487e39c222a58200244b5"],["/categories/中间件/index.html","d296626069d789dfc7f5864d2ebec215"],["/categories/前端/Vue/index.html","debc843c5cd2c9e7275d57f3f540c3f4"],["/categories/前端/index.html","eb1f7123c93017390c89dd148bdb913a"],["/categories/大数据开发/ElasticSearch/index.html","d191a6af4ce30f33f97dffb2c4bda777"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1f42364aeb9d99fad45d335527a13f99"],["/categories/大数据开发/HBase/index.html","da858e801bfde72c56e278d2fab10605"],["/categories/大数据开发/HBase/学习笔记/index.html","1cbe3eda916c115942b0938450c6c60e"],["/categories/大数据开发/HBase/环境搭建/index.html","b18a214021ad94fbe682262e5f309a4e"],["/categories/大数据开发/Hadoop/index.html","6ab630c24cad80e208a755f9ec5b059a"],["/categories/大数据开发/Hadoop/技术/index.html","03d1ed479a150095fadaada4ce5f2a67"],["/categories/大数据开发/Hadoop/环境搭建/index.html","8c86b1ac094cdd5c197fa59beacee661"],["/categories/大数据开发/Redis/index.html","e9eba62cd36cae07dc1265f167600786"],["/categories/大数据开发/Redis/技术/index.html","cedb3fb2176135558cb1fe19346abafa"],["/categories/大数据开发/Redis/环境搭建/index.html","7e231394b73f510c5090526addb66dd6"],["/categories/大数据开发/Spark/index.html","a853395d82c3ffa02d4b36875b862a91"],["/categories/大数据开发/Spark/环境搭建/index.html","edffce2543b695fac00ca11d90f64560"],["/categories/大数据开发/Zookeeper/index.html","9c5edc67e9dd89440ebf00a89d957658"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e91bbf6964472790706bed8331512fb2"],["/categories/大数据开发/index.html","88ae4a9f080b8d5ce8fc4ff825922ade"],["/categories/学校课程/index.html","9a32e5620ad9d8512f1be0f88f3fffb5"],["/categories/学校课程/计算机操作系统/index.html","4b92b0fceb75920c6f3a0ac427569678"],["/categories/操作系统/Linux/index.html","839a4245f9799703683e2bb05f6f892f"],["/categories/操作系统/Mac/index.html","664ce57d7e100f8df9420fe395ba5750"],["/categories/操作系统/Windows/index.html","ea5f7452e5ce7d532c5a074dce2aebe8"],["/categories/操作系统/index.html","b144e7b9f46dc6607edb14979fc09a59"],["/categories/数学建模/index.html","245560968874e00daf6aa1528c8f7fc4"],["/categories/数学建模/latex/index.html","bdd4cf03c2694981583696359d2f9fea"],["/categories/数学建模/优化类/index.html","2bc8e89386c04e1a7fd3db4edd616d10"],["/categories/数学建模/优化类/现代优化算法/index.html","077647cd769ade373a9f96972c4aab0c"],["/categories/数学建模/优化类/规划类/index.html","d3b34544e013646acc6a2d142f8ddba2"],["/categories/数学建模/绘图/index.html","6b26fc6509f9356a67e3fbc89473f244"],["/categories/数据库/MySQL/index.html","6bf998919a0c82bcbe51f97a730f54ad"],["/categories/数据库/index.html","d7c2586c83211b95c025cd7b26318dbc"],["/categories/数据结构和算法/index.html","d0d15eaa3a66a3e6eb53f494b6f778ad"],["/categories/数据结构和算法/page/2/index.html","79ee5daa852e48c48b37edfb661c53d3"],["/categories/数据结构和算法/基本原理/bfs/index.html","ac583e7990fc864d565b53e6d558e870"],["/categories/数据结构和算法/基本原理/dfs/index.html","7edb8a68782fbaa1978bf8bd3a02745b"],["/categories/数据结构和算法/基本原理/index.html","4ac2a3ab29a7793420c935bac4cdab92"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4d3a8611b0a0bab62925349b44cb9381"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ab9a1bf51b7f29b8a906703537b8ae6a"],["/categories/数据结构和算法/基本原理/图论/index.html","8ed930073a3e41f37f1568a1ebaaeb97"],["/categories/数据结构和算法/基本原理/字符串/index.html","408f55429a0810aaab295cae6277992f"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2558d150b5d1afe905979bea63d1a96d"],["/categories/数据结构和算法/基本原理/数论/index.html","cf30b11db4ceacd2261f653cd4372517"],["/categories/数据结构和算法/基本原理/树论/index.html","76b29316d3c98149e20e3f20a85aa261"],["/categories/数据结构和算法/基本原理/链表/index.html","1b82f4abbe1985b1f8cdef533c089f57"],["/categories/数据结构和算法/算法题/index.html","b1f1423414d13483ad2fe36e28570c26"],["/categories/数据结构和算法/算法题/二分查找/index.html","c9941d93ce331661ddec500be8bece20"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4edd02a5d37bab2f2dd168ab09ac9ba0"],["/categories/数据结构和算法/算法题/动态规划/index.html","03c3cf6143b6b1a1143d181fb34c6d06"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","dd8fd4692166f3ae7aad0c74e8742d6f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","567da65c36f4eb5bd9ec228119c1c308"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b62cf68ed5fab9c6c6d16e9fd8200b50"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c0ba51161c4758a65a13b293cbc51232"],["/categories/数据结构和算法/算法题/数论/index.html","24d38208aed15848bc4f1c7c8ae6e3e2"],["/categories/数据结构和算法/算法题/栈和队列/index.html","683b6b56d03805a00fd90ad621902ac1"],["/categories/数据结构和算法/算法题/树论/index.html","1011af7163bbcf6e27daebee0fcf0147"],["/categories/杂七杂八/index.html","141b7383888463aba210fb4fac15e361"],["/categories/杂七杂八/博客搭建/index.html","0dfc87060a426b184d7abe5ddaff7f84"],["/categories/编程工具下载/index.html","f9334838a6b3680595b2a7e4b2bc2c45"],["/categories/编程环境/index.html","1d08720f6eeceff14b8151cf7ab728ec"],["/categories/编程环境/大数据/index.html","6daebc834b7307054826e93e0a50ea79"],["/categories/英语学习/index.html","b4475b7ccd937000bd5a6e1fc905604c"],["/categories/英语学习/英语语法/index.html","b283176b5cac234df9813bcfda1899c6"],["/comments/index.html","ee9fd1bd32291f3db2d9dfcb08f80b12"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7508b4cc91d10b10a2142f03bcda1c8e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","571cdeb465252074252bef81a9f753de"],["/movies/index.html","f1a25abb741cb094e41cc9e673fb2101"],["/music/index.html","4bb4e039dea4715c407b5fc8be580318"],["/page/2/index.html","9c30eab122fe3851aea777232b8736f7"],["/page/3/index.html","65f2c91cb91415810ea4f8ac58d635c3"],["/page/4/index.html","9631851613b2e55d65cd74013d928ffd"],["/page/5/index.html","8301f18ca9805c6cea3ffc745a34728e"],["/page/6/index.html","fb171573e7d35159644641b760bea2ac"],["/posts/1021360842.html","6cd4e1607a48850d2213192b572a04ec"],["/posts/1120620192.html","41cf22c78f1736c51dde04f682203368"],["/posts/1141628095.html","def56763cde622538e736c790f0150f1"],["/posts/1168613674.html","f50558c8e33f0638a25db7a4d0d05b90"],["/posts/1219920510.html","04bcc4d64264bdebfd3d7e221c49b2aa"],["/posts/1222166338.html","13dbca6b6572e527826d7d5c6557ed74"],["/posts/1259097482.html","0b3b55bd788f0647ce709fa6c29532d4"],["/posts/1271036369.html","ec6c631aa3a91b8fc1fd0b4e4abdf88c"],["/posts/1312847445.html","262b0ce029a4b4ba2c0c966b4c561f66"],["/posts/135355774.html","69d60cd23e016eee82992938a0abc38e"],["/posts/1375344716.html","c1e6d9c0b5e2cf6b2deed53344dc9008"],["/posts/1388991698.html","979bb1200b1520b168472e5557d0e80b"],["/posts/1410315814.html","18b1e7a3e9d4466963009939314573f2"],["/posts/1452790229.html","e9b8ac20f3b162debe1420df8723af93"],["/posts/1470079884.html","9c0819ac52133a84a8622350b90f414f"],["/posts/1470079885.html","e39af5c3d4de8a7085b14f7781c0926a"],["/posts/1470079886.html","894d9e5a56a45490f75be197bc7b0925"],["/posts/1470079887.html","97551d19257ef15589c12ef677d2f6f0"],["/posts/1498536549.html","b82179bf7258afa5bb2a3fefb56a1bc5"],["/posts/1539568593.html","23bdb8799e3025d6507fc85a38c80cf0"],["/posts/1547067935.html","b4e9a857af12229db3517a8185a12d14"],["/posts/1557866301.html","9cb9d4e486be6609e51e75d0598a859f"],["/posts/1571776361.html","6a12a8baf66033f3fe7af82616663a3f"],["/posts/1605124548.html","f8e1dd311bc89aea9c64e315daaec2ba"],["/posts/1633036852.html","a20cae720afec04a98b7ec4c3abab95c"],["/posts/1674202625.html","c26148f20ca26b43acca21afdde9a2e0"],["/posts/1765123828.html","33da9a0172d896d86d03db83722fe276"],["/posts/1767336200.html","97986a1f192cdc53b2545c4d734d8a75"],["/posts/1776114197.html","304dde9f84ce42a1ba330b619772b3f9"],["/posts/1817748743.html","68c6955341287ca3c4d0927cee61e81b"],["/posts/1925125395.html","f520b58855028549d70ce927f27ffc96"],["/posts/1966191251.html","0072ac5e4e1811ee3460a6fb817d8f85"],["/posts/1987617322.html","7b9107813046fb826fcd48a359bcb677"],["/posts/1999788039.html","35350153cb723529eb4230092d9a3020"],["/posts/2075104059.html","7aa3fca6baf7470e139b3e664058b6b6"],["/posts/2087796737.html","3e479d5a54885bb88ffba64aef899080"],["/posts/2106547339.html","df4527153a06673f261f7ed9e2871d2e"],["/posts/2207806286.html","36006f3759c7bd0ad73f28fabbc82038"],["/posts/2225903441.html","5f8352b2b2903a807e7acbefe55cbb72"],["/posts/2265610284.html","b0b3bc2f718967647268721d58f08f02"],["/posts/2281352001.html","0c1d92ff1557403465c619c3f23efdc3"],["/posts/2364755265.html","b342ddb0802752fd29e550e255e1b24b"],["/posts/2414116852.html","19ac73e731b55b4f391a1ba264b5d80b"],["/posts/2421785022.html","a9ecf65a26426923bf6bf5e1e4f8a610"],["/posts/2482902029.html","5c266a847b4ba59476279cf783a88b50"],["/posts/2495386210.html","a9104d55185df9e06107145db850823e"],["/posts/2516528882.html","2a2317763f8e0603406d41a0226f27cd"],["/posts/2526659543.html","dbb437f7ed4065caf0568b283583cafb"],["/posts/2529807823.html","1373f76ed8ed79dd10251f06158fd51b"],["/posts/2596601004.html","f29fa4eb2e1f40157b0f5bf08a62430b"],["/posts/2697614349.html","c330c89057c99eedfb41f39939cd6e51"],["/posts/2742438348.html","26c6f028c9b86c7f82ce404a22444909"],["/posts/2768249503.html","5a5c22ebaa5ce60e4090d1bb0825798d"],["/posts/2864584994.html","2765f13eadf3fe331d77834116b37a47"],["/posts/2888309600.html","919f73a32550734f3f7132bb75e64039"],["/posts/2891591958.html","4d89af251cde269b1704adee5921aaac"],["/posts/2909934084.html","6cd24281042b779a7c1182f449c10c76"],["/posts/2920256992.html","79ed663daddd901154aee5d0a2a6ec17"],["/posts/2959474469.html","5cfb76a9669faad428c39a8d1087095d"],["/posts/3005926051.html","60951c7cf56e4767966652a2787dbef9"],["/posts/309775400.html","2a24a48e21436a2b7bfa4ff1e2dcad6e"],["/posts/3156194925.html","5bacc523a1b30dbb70d64bb4d5c7b3a3"],["/posts/3169224211.html","1cf6a55b6d405190d320c4dede388e7b"],["/posts/3213899550.html","8765a060deb8f477233a0daa4c73f53e"],["/posts/3259212833.html","a785ad95905f0d675b89b99d5a376fe1"],["/posts/3266130344.html","83afa54493b30942d209147b7e3d1fa3"],["/posts/3292663995.html","11be809f831e78bb33f3b111eddf0a10"],["/posts/3297135020.html","2170851b00ae2d1ea72993e5c42fdafb"],["/posts/3306641566.html","996c38c1381fd0bfb7825c09e287a766"],["/posts/3312011324.html","0f3aaf368b8a913e740f4814f6bf5f48"],["/posts/336911618.html","eda9c5990f67dcf1847cf13c4a07fc75"],["/posts/3402121571.html","8543f96e451d695cbbdae7b0f5141216"],["/posts/3405577485.html","3744bde3aac728de44fe60b82b706081"],["/posts/3498516849.html","405b1d3546b0b49d0f0dff0b0cc3f0f4"],["/posts/3513711414.html","adc7e2133d5ef868a087830cf2d317c2"],["/posts/3523095624.html","610f7e2db6557045249821c0ffd6e80c"],["/posts/3546711884.html","39c6c252675729fdc389ad36fa34b46a"],["/posts/3731385230.html","60a9565eee36c904ff13b82385410d11"],["/posts/3772089482.html","9a887b04e8eca40cfeed6133cd0c3d14"],["/posts/386609427.html","6fdf67453c294667cfa51806398141cc"],["/posts/4044235327.html","f1917ad6dae14ac9929c841b80eec875"],["/posts/4115971639.html","6671fb95400ec8147f764f12950a75ab"],["/posts/4130790367.html","169e23057d87182989d96f297a6d1f8c"],["/posts/4131986683.html","17ab595d21691301102b89fe9007be90"],["/posts/4177218757.html","e742defcdb0da90c722314b7516b8674"],["/posts/4192183953.html","a4f9a0a2a0db830d8fa5235bdd304a21"],["/posts/4261103898.html","e7b10ef0b488c6ac51b7507fcf314560"],["/posts/469711973.html","0b8221b699cced11a88b305779932b41"],["/posts/482495853.html","643b4db764c0f0e53f5d88dc6cf25f34"],["/posts/488247922.html","23abb5daacc0c68351296fed8fa91851"],["/posts/517302816.html","433b85d22cf7c4b3401879e3ad5d2bb5"],["/posts/570165348.html","cf80ccad371fae3651d3dc8357bf6489"],["/posts/595890772.html","10563c17e0cadb000d454023942fc228"],["/posts/67485572.html","12992b71af2c446856130239213d190c"],["/posts/694347442.html","f48d6843130fc7671751672f9f626c6a"],["/posts/707384687.html","e55a4aaaad9474a284f5cdbc6267ef6c"],["/posts/71180092.html","ef2f30b83ec3ec36a8ac10402cffd236"],["/posts/716459272.html","d76b14b0f09cf1eb481ef04cccb37658"],["/posts/765481613.html","b7c37bb65f8ca23761e746c9c7d8a32a"],["/posts/778231993.html","1004ed04d66ff4de8625fa090e97afa3"],["/posts/795397410.html","c12a998faa6266e5f0f98c148d8c9fc1"],["/posts/820223701.html","b4d3b1a957a668a644b1f66053b34b67"],["/posts/830372185.html","f4522e3f970e742d924437457c0987c9"],["/posts/88294277.html","8663157428236c7d56d7a4bfe1854acd"],["/posts/939963535.html","e78b2a4e56c483c6093b60cdb6602bb0"],["/posts/983786067.html","5b6e7985d352840230f9fdab2d97318f"],["/sw-register.js","1489198548498d7aa1b1828e3798c60a"],["/tags/C/index.html","ef615a85d2f0721523ed9dd72d0730d7"],["/tags/C/page/2/index.html","13dc12c00c09af91337078f41b6e4cac"],["/tags/C/page/3/index.html","6875acbf0993bb86d018af026fcfaaf4"],["/tags/C/page/4/index.html","b9841c3736df8641ec2b077c7461cc14"],["/tags/ETL/index.html","916318853a61a7f3b2de48b20568f7b0"],["/tags/ElasticSearch/index.html","0be390b03fe05de1ad92fd1cd5ac78b3"],["/tags/GUI/index.html","b755d5c0b4b217bf510986d046dffb75"],["/tags/HBase/index.html","c831064d2f7d0ea140424d4e6c7aa196"],["/tags/Hadoop/index.html","146dc938064ad0b34b0504455ec9288b"],["/tags/Hadoop/page/2/index.html","6a6a01ff34c3cf5c0633edce0cc16193"],["/tags/Java/index.html","bc784ee48dd282e5ef6c7ec22751488c"],["/tags/Java后端/index.html","2712536fc24681e913c174acfb2eebd3"],["/tags/Java后端/page/2/index.html","d6074c49db423bdbac88b877c12696b7"],["/tags/Java基础/index.html","0153e90fcdc273ef05a9d206f6f839e0"],["/tags/Java基础/page/2/index.html","100188d9644a863ec15aa7733c3c9a86"],["/tags/Kettle/index.html","b95980d924c8df4d53df13afffae1789"],["/tags/Kibana/index.html","df93e6dc9df83c5569f42e9b4d7664e1"],["/tags/Linux/index.html","c2fb6acf4859a0907fc7d33c79b57186"],["/tags/Linux/page/2/index.html","02ffb78f3d2da167ef1a054b19434a5e"],["/tags/Linux/page/3/index.html","01255bbe0e058ceceeb04aa13f89385f"],["/tags/Mac/index.html","1664d34516d9228feeda8a4c2390001a"],["/tags/Mac/page/2/index.html","e9df75d5036153d190fa1bd075fae7e1"],["/tags/Maven/index.html","2eea09b1f0aee409b9fa03f36adea5cd"],["/tags/MySQL/index.html","7ae038bee06705fd13e26212e0b315a9"],["/tags/Python/index.html","fbd2eae7c7cb06413047ee21d2719fb7"],["/tags/Redis/index.html","d3ad8d8f079681ac980ee5c0dd5fd397"],["/tags/R语言/index.html","51bbb2603dcab42b92c8179baf69dd96"],["/tags/Spark/index.html","ac424899e778df60221380e65143528a"],["/tags/Ubuntu/index.html","c913dc13262030b9a1b41cd2a885091f"],["/tags/Vue/index.html","7f654e7f79e9051a4ce5c7c703e7e9ad"],["/tags/Windows/index.html","75eef062f95f5679a0acacd36061e8f2"],["/tags/ZooKeeper/index.html","414fbf3d416fe1aa8f04dc8e3f147b98"],["/tags/bfs/index.html","7bc230bbfdd9d1059387ba5d399cf45b"],["/tags/dfs/index.html","5b918508875f415aad29e73959c09f88"],["/tags/folium/index.html","7576b442d39514867cee0aed6abd5ba9"],["/tags/git/index.html","88586a3bc016aa30150dd9fb7a963c25"],["/tags/index.html","e9f8a31e27c93fe8826ee08fe9ebdc7c"],["/tags/latex/index.html","68880973166fb74c9a5cc4a8e20aaf17"],["/tags/中间件/index.html","d009fd6f5e37e33a2ffb70a56e2e6d6b"],["/tags/二分查找/index.html","2d73fa93e54f851ceb1da711f379c064"],["/tags/优化类/index.html","43470d73f3dafe983ffa5fb0f5f668ee"],["/tags/前端/index.html","ac6ae04ae073c357dbb64da5aaf1f62c"],["/tags/前缀和与差分/index.html","531139a8d412033b9de14e42e1c6fe41"],["/tags/动态规划/index.html","3166f460a37ab41b5f53bc642cc442f4"],["/tags/动态规划/page/2/index.html","8288af14b5705e0ea93ff416b34e9eff"],["/tags/博客搭建/index.html","18307f79d201ee0f3b78f85ff7c5dad5"],["/tags/图论/index.html","e30b2bb350535192f113ec24da019713"],["/tags/大数据/index.html","dab79949ba1e7cafe3ca459ee9a4e2b4"],["/tags/大数据/page/2/index.html","786264fd4660a90f6c148411d2aa8172"],["/tags/操作系统/index.html","d42dfa3ac0eaa4140ceb8200051eb225"],["/tags/数学建模/index.html","e86ebd7e1f425ebffa26beab2ecd1824"],["/tags/数据库/index.html","81a504a787f0fc0e98e938122aeb8684"],["/tags/数据结构和算法/index.html","a35cccda025426c88377268598d39fee"],["/tags/数据结构和算法/page/2/index.html","8b04f3648cf37898bf5eb3c5868ee723"],["/tags/数据结构和算法/page/3/index.html","3e9ca5d14a69beaa131169b10bc9d098"],["/tags/数据结构和算法/page/4/index.html","341745b411ca8e12dd556d1ef8a2cfcc"],["/tags/数组和字符串/index.html","7e70e69f91633670270ab31e8e08d624"],["/tags/数论/index.html","dc6ed73a931df27b7466bf7bac0ce0ec"],["/tags/枚举类/index.html","ad5d34de376262946a7d4895fb173293"],["/tags/栈和队列/index.html","eec302a4fadda6a54c001a1b009009cd"],["/tags/树论/index.html","95bbcded95603f9a0ef11fcfe69daff9"],["/tags/测试/index.html","43a1b8f17e94ba4af537c1b4bcdcb1af"],["/tags/环境/index.html","c92ff8b83c2c478dd4deb03462db53a5"],["/tags/环境变量/index.html","764e2f2db2f086dc69618eaeb72ed7fe"],["/tags/绘图/index.html","3a5529255fbe640853bfa54fb3356e4a"],["/tags/编程工具/index.html","7bb4d0e14fab6b0a39fa5f8b073fc883"],["/tags/编程环境/index.html","4174bd170c6ac97470a8e6f90c9b039c"],["/tags/网络编程/index.html","b96a51f5799aa84ef0a0b307b37ffa94"],["/tags/英语语法/index.html","b6bc92081c80a43571b69dcd3bb84e19"],["/tags/计算机操作系统/index.html","07414a01fc72d4403e3126abfb252840"],["/tags/论文/index.html","aa50f951be3c19921dbd58df55b88650"],["/tags/资源下载/index.html","371ac337025376f0ed54a84c59f4102b"],["/tags/链表/index.html","aba98374a850896f071584489220c317"],["/tags/集合/index.html","44dba783e7808863794759db664abd05"],["/tags/集群/index.html","90f89e411e4adaccf8151300de1d5267"]];
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
