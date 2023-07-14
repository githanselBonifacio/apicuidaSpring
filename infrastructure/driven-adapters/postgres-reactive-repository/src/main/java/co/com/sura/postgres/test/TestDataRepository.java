package co.com.sura.postgres.test;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

@Repository
public interface TestDataRepository extends ReactiveCrudRepository<TestData, Long> {
    @Query(value = "select * from tbl_test")
    Flux<TestData> getAll(PageRequest pageRequest);
}
