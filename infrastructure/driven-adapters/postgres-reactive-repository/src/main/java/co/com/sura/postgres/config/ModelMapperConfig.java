package co.com.sura.postgres.config;

import org.modelmapper.ModelMapper;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ModelMapperConfig {

    @ConditionalOnMissingBean
    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }
}
