package com.sura.reactive.repository.jpa.testdata;

import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.QueryByExampleExecutor;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

interface DataRepository extends ReactiveCrudRepository<DataEntity, String>, QueryByExampleExecutor<DataEntity> {
}
