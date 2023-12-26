package co.com.sura.web;

import lombok.AllArgsConstructor;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.server.WebServer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.http.server.reactive.ContextPathCompositeHandler;
import org.springframework.http.server.reactive.HttpHandler;
import java.util.HashMap;
import java.util.Map;

@Configuration
@AllArgsConstructor
public class WebEntryConfig {

    private final Environment env;

    @Bean
    public NettyReactiveWebServerFactory nettyReactiveWebServerFactory() {
        return new NettyReactiveWebServerFactory() {
            @Override
            public WebServer getWebServer(HttpHandler httpHandler) {

                Map<String, HttpHandler> handlerMap = new HashMap<>();
                handlerMap.put(env.getProperty("app.context"), httpHandler);
                handlerMap.put("UTF-8", httpHandler);
                return super.getWebServer(new ContextPathCompositeHandler(handlerMap));
            }
        };
    }

}
