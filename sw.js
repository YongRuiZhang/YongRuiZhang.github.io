/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","db20d1da68b7f96c3860d0f48f649e4f"],["/about/index.html","1334b59d3ebfe5cd35f81c3cd3640edb"],["/archives/2023/01/index.html","0627e5677d3f802e35a51a2e85e6c0d2"],["/archives/2023/02/index.html","dd4bc8b8e064ddb6ba614b0d38955d5c"],["/archives/2023/02/page/2/index.html","e460ce199812cfd57ef02c8008d11a7b"],["/archives/2023/index.html","0023a89d2c58e573d8f27f962281988e"],["/archives/2023/page/2/index.html","fbaec6e22e057ef7826b705ba94430cf"],["/archives/2023/page/3/index.html","a2d8d805959657cb53c99e168663ce8c"],["/archives/index.html","a1bbc95a5bc8acb312c36f92072a1026"],["/archives/page/2/index.html","3033e1fc05419d830ef73cf820c4071c"],["/archives/page/3/index.html","ac382125ebc475351131a864a825c7b2"],["/categories/Java/index.html","ab9bc33647e0ccd28516d9e8f08edb3b"],["/categories/Java/后端/index.html","3e2601b54f2965111d03f9dc21aff802"],["/categories/Java/基础/index.html","ea22ac5f10dcb30089b690b81f606417"],["/categories/Java/基础/集合/index.html","61a0d1d61f2da6e650ed495e7e2fc18c"],["/categories/Python/index.html","db6a46a1d59b1e87c0677b64299911bd"],["/categories/Python/编程环境/index.html","39990f7463e9752023cd9e154fd2df49"],["/categories/index.html","4a0bc6424460184e2aac740d995d7745"],["/categories/大数据开发/Hadoop/index.html","fd941e61e2f039d8a21a1fcee77c799a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1c77a73365e8754fa653a73150496d2e"],["/categories/大数据开发/Zookeeper/index.html","4618f773e82bacc71d99cf39e76a0559"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c62f97410534b149dbc924588f0ae0ac"],["/categories/大数据开发/index.html","74b6fa56cfb60b715e8a22e4c6b7d3f4"],["/categories/操作系统/Linux/index.html","f17d8edae4ae1751263b54a3a66258db"],["/categories/操作系统/Mac/index.html","d160b1c0a58b32f886101930bbed2e7e"],["/categories/操作系统/index.html","43ed6ad245635f023b5aa700bf1aae11"],["/categories/数学建模/index.html","3388aacd09944e082915677e0cc1c82e"],["/categories/数学建模/latex/index.html","844657b7ef7c566487026b34e7bfa325"],["/categories/数学建模/优化类/index.html","c6738a418c32a41bbf1cbe6659fb749f"],["/categories/数学建模/优化类/现代优化算法/index.html","875162df65891fdc03180060ff373eeb"],["/categories/数学建模/优化类/规划类/index.html","f73d9c6cd6ade1cc5ac230c42458e0ad"],["/categories/数据库/MySQL/index.html","a101723077407c78b6e6ea30b0cc0374"],["/categories/数据库/index.html","6c42f4677e09504b4230ccde41f0d4a3"],["/categories/数据结构和算法/bfs/index.html","cfec9f757ab00f629301ccb8f51be369"],["/categories/数据结构和算法/dfs/index.html","3c658f3216b66747270f79c071e6734f"],["/categories/数据结构和算法/index.html","606ee19c8825172f4417967d6929f4f0"],["/categories/数据结构和算法/图论/index.html","eec40cab226087efc2513e5df9a3f45b"],["/categories/数据结构和算法/数论/index.html","13b0c8815f61879f439f05ae320e47b8"],["/categories/数据结构和算法/链表/index.html","9d61d557aad07da9941e6fe1da734ef8"],["/categories/杂七杂八/index.html","bb3f663077b45890498fed08156085c1"],["/categories/杂七杂八/博客搭建/index.html","9e1a662f00b04d684c25007885f11843"],["/categories/编程环境/index.html","062b3920033cd4eefdd7bb7658c28697"],["/categories/英语学习/index.html","efdf5cf87c1ee77558af5737eb8b05aa"],["/categories/英语学习/英语语法/index.html","0b96759c38a04472afca6917d2108c11"],["/comments/index.html","e3b74899662f61fe8ba0c0bab0d86ded"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2189ed83b99df64bd483ee0bae64c05b"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","f00e9fc8941c8e445417f1faa85f182d"],["/movies/index.html","cac4c556c7d4c7bd516756b1164556fc"],["/music/index.html","0f3f2923095fdf6b6cb9323183022186"],["/page/2/index.html","df501d361fdcadc85f9162ba0a162aba"],["/page/3/index.html","ed88a5db38819ad84502a99adb3e8cf9"],["/page/4/index.html","7db20addd1d98d1e16e3c1e6d8f607ef"],["/posts/1021360842.html","d1acbf6c288d4f635bdb7f279f488e38"],["/posts/1120620192.html","0e2fee5a188a57ce8a0a9791770fcbe8"],["/posts/1141628095.html","7f4646f8dbbb6db80ce40532d0f06bd1"],["/posts/1168613674.html","0bce35f9bc06313f63da801b3eb55465"],["/posts/1219920510.html","a0e98fd0e88dd2c77007ffdea3473428"],["/posts/1222166338.html","ace7cde66b5285e9018146a7ff005742"],["/posts/1259097482.html","7217035e41295d840ab9116cf5521184"],["/posts/1271036369.html","82785b0b4798858ad979567b1e177706"],["/posts/135355774.html","67515456208ad0cac7a3d3ac854efb60"],["/posts/1375344716.html","0ea303943d2c45204ebaeebab19c1944"],["/posts/1388991698.html","1840f0d888ddae7578c2c24c0158d771"],["/posts/1410315814.html","24285824371bd346037e0625f7cc2002"],["/posts/1470079884.html","f9a9e019b1e718833f482dffe06d7adb"],["/posts/1470079885.html","f2d85e9c57447f5089883be02f0755ba"],["/posts/1470079886.html","2b7e3492db85fb89682256763ffb4d93"],["/posts/1470079887.html","99f5cc7451af1dd532e440de3d76175b"],["/posts/1498536549.html","e070a98a5fd3cf3a9088d5d621aef09f"],["/posts/1571776361.html","229dfe7bf59395fcb8cab02bcafc963f"],["/posts/1605124548.html","05124a3e18cb67ae7dd74c0779603176"],["/posts/1925125395.html","4473ca7c86f449a067beb85c911236fe"],["/posts/1966191251.html","7f94998c52d93966e26037669d82b565"],["/posts/1987617322.html","0eaad50c1dc3e9dd612e9e27e63d9ce4"],["/posts/2075104059.html","ce5e8238c355030a46ff828a9c51045e"],["/posts/2087796737.html","4338d24914880d7c927420fe635b6687"],["/posts/2207806286.html","438aed7e6fc35b734da743619397fa0d"],["/posts/2364755265.html","89ad2174cbc72dc53235e27fad6cb913"],["/posts/2414116852.html","2c26a4c5b64d093bb2865bddb05767b5"],["/posts/2495386210.html","37fd30db8a16c60491c4906ba2f11728"],["/posts/2516528882.html","8f2be9d9d9670fb1aba536a45458bbb7"],["/posts/2529807823.html","e2ae412ed44f421bc30fa1e9ff944f19"],["/posts/2909934084.html","5164d975841e1efedc0f86707dbec307"],["/posts/3169224211.html","66d0cd3b84635ada8ae344b192add908"],["/posts/3259212833.html","637b11eada33dfff4646ab436d38e4ce"],["/posts/3266130344.html","e83c97ebf10579d89060b5d252dd3a74"],["/posts/3306641566.html","c9e1f48df8271e81305cdb2a749eb1f2"],["/posts/3312011324.html","4385597613ba669bc6cd690c7a501ee4"],["/posts/3402121571.html","7eb0de912ffc0d6f764327ea04be2b03"],["/posts/3513711414.html","0f3bb3ca87249e15a5c0ef404b4874ee"],["/posts/3546711884.html","e6d0a81ab1b33e92093d0e7c3eb6c1b3"],["/posts/3731385230.html","5796956ff111d2da4eb842a67f9d15dc"],["/posts/3772089482.html","fb401153a8f1fc97096eafb095b9008c"],["/posts/4130790367.html","9550b6c4921f240621e2a3e292bdc1cd"],["/posts/4131986683.html","f5a3407a1d5c0e246d27cae64d4f8fd0"],["/posts/4177218757.html","bf772ebbb769d34bb482d137d08bb4bb"],["/posts/4192183953.html","8c5fcdada4c9751b8d541910070c23d7"],["/posts/4261103898.html","5a3106e3c980410b86821f3a50402bef"],["/posts/482495853.html","da75f89b7e635bd5fa01f054aaf68cec"],["/posts/488247922.html","a9858554d4de4e8929769e1c65832e03"],["/posts/570165348.html","1d276a120231f1fc89758658d3f1cd80"],["/posts/595890772.html","2461c65a24278ab38f990e471fb51091"],["/posts/694347442.html","e6c4a727aa463a17ecde3d8d4cbf979b"],["/posts/707384687.html","5b69ca84b72a424bed36c564169559aa"],["/posts/820223701.html","cca5e335d28ebf1d0ef511fc40d1460b"],["/posts/88294277.html","b346f263e57ebf091857e5ece69da90d"],["/posts/983786067.html","70c9fd1d15933d4c02742cabdb5c08ff"],["/sw-register.js","aa7c2953613dea5aa8c48582e1ac95e9"],["/tags/C/index.html","cb70c317c61d7b478b0ca66627c356c7"],["/tags/GUI/index.html","cc976326acd26b47f9f745f7f83e5714"],["/tags/Hadoop/index.html","e6f3edf62d0d8bf0b1e3cbf525717ff5"],["/tags/Java后端/index.html","c1d6d54def8da37f0f2ec68aca67b81b"],["/tags/Java基础/index.html","a0ef5ef27673b42ccac580604d780b8a"],["/tags/Java基础/page/2/index.html","23ca176b5a1a617e5ce84ac8718d7033"],["/tags/Linux/index.html","16619b174a5edde030dce4fc8516b7bb"],["/tags/Linux/page/2/index.html","d5b337929eeae0605d41b8832c61ef31"],["/tags/Mac/index.html","965e40101aa676be805a90665f7ff95a"],["/tags/Maven/index.html","a3a29eb8bb4a2126126e1e283afc16bb"],["/tags/MySQL/index.html","b60da1539ad89bec08c4acc72cdc6898"],["/tags/Ubuntu/index.html","512dd1071c69d370d0902335a0781ea4"],["/tags/Zookeeper/index.html","07d0a94bf732bd459c44ca3776e7c5b9"],["/tags/bfs/index.html","bc377bfbcca09bdf33de44f235a7bc84"],["/tags/dfs/index.html","235aaf55fa73f22a4e204d63b6dd3c12"],["/tags/git/index.html","924dfa1e283e76d8584dd75686296458"],["/tags/index.html","522af5ff7b22452f5eba960f3b5520cc"],["/tags/latex/index.html","2e809f27712a734dee09fdf7e07d8233"],["/tags/python/index.html","8073c9e87b45212e58bb9d54a243f679"],["/tags/优化类/index.html","14c0b5dc350399919357c3088754cd9f"],["/tags/博客搭建/index.html","e1d2bfe186698c2bdf0d871cd6753ffe"],["/tags/图论/index.html","e977130f4886514574a090d63203f39a"],["/tags/大数据/index.html","de39f4dd687b77add0f437babadf2215"],["/tags/操作系统/index.html","032e27967d858d9b759646ee4c254b7b"],["/tags/数学建模/index.html","67ececaa89a6615afcb9bf1d72d347b3"],["/tags/数据库/index.html","f635f65ce384f530f1450eeb20dcb336"],["/tags/数据结构和算法/index.html","2c5d06367026f1b6eaacb0c2c8cb786a"],["/tags/枚举类/index.html","329fc68cd2c754f2c310344c62891d04"],["/tags/树论/index.html","7e336390bc6d1c32f04c5a8a1214e8d1"],["/tags/测试/index.html","41285a6b3c137e9f3683a88852823605"],["/tags/环境/index.html","f19bd04095a5e93525ea4a072b484b05"],["/tags/环境变量/index.html","1a7fe5a09f981fbf8800b69a6ac19a20"],["/tags/编程环境/index.html","d6a7c9311d3d11ac29aa510d0f4e4155"],["/tags/网络编程/index.html","fb7b065ff527e6241c4134f6016b13d3"],["/tags/英语语法/index.html","25d9cb8c2747066a0ecc32b1bf77526d"],["/tags/论文/index.html","f0df152ecdc76bd6dbf0a51611e962ed"],["/tags/资源下载/index.html","a0a608ae896d1b4f5d6bc1d648d4d970"],["/tags/链表/index.html","d371fb0022443bdf08c299580bc356e0"],["/tags/集合/index.html","80e21b4c485491c7c7173baf76550e9f"],["/tags/集群/index.html","594b66700468570e62aa81e6232617a5"]];
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
