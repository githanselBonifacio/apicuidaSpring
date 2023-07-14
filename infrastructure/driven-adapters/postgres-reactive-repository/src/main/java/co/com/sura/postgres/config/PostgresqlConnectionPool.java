package co.com.sura.postgres.config;

import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.r2dbc.repository.config.EnableR2dbcRepositories;
import org.springframework.r2dbc.connection.R2dbcTransactionManager;
import org.springframework.transaction.ReactiveTransactionManager;

import java.time.Duration;

import static io.r2dbc.pool.PoolingConnectionFactoryProvider.*;

@Configuration
@RequiredArgsConstructor
@EnableR2dbcRepositories
public class PostgresqlConnectionPool {
    public static final int _INITIAL_SIZE = 12;
    public static final int _MAX_SIZE = 15;
    public static final int _MAX_IDLE_TIME = 30;

    private final DataBaseConnectionProperties properties;

    @Bean
    public ConnectionFactory connectionFactory() {
        String url = "r2dbc:postgresql://"+
                properties.getHost()+":"+
                properties.getPort()+"/"+
                properties.getDatabase();

        return ConnectionFactories.get(ConnectionFactoryOptions.parse(url)
                .mutate()
                .option(ConnectionFactoryOptions.USER, properties.getUsername())
                .option(ConnectionFactoryOptions.PASSWORD, properties.getPassword())
                .option(INITIAL_SIZE, _INITIAL_SIZE)
                .option(MAX_SIZE, _MAX_SIZE)
                .option(MAX_IDLE_TIME, Duration.ofMinutes(_MAX_IDLE_TIME))
                .build());
    }
    @Bean
    ReactiveTransactionManager transactionManager(ConnectionFactory connectionFactory) {
        return new R2dbcTransactionManager(connectionFactory);
    }
}
