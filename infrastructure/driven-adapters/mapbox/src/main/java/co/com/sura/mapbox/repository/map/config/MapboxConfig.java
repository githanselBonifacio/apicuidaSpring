package co.com.sura.mapbox.repository.map.config;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;
import org.springframework.web.util.UriComponentsBuilder;

@RequiredArgsConstructor
@Configuration
public class MapboxConfig {

    private final MapboxProperties properties;

    @Bean
    public RestTemplate restTemplate() {

        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(properties.getApiUrlBase())
                .queryParam("access_token",properties.getAccessToken());

        var restTemplate = new RestTemplateBuilder()
                .build();

        restTemplate.setUriTemplateHandler(new DefaultUriBuilderFactory(uriBuilder));
       return restTemplate;
    }
}
