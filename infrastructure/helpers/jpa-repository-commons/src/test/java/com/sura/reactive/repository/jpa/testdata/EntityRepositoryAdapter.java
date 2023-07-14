package com.sura.reactive.repository.jpa.testdata;

import com.sura.reactive.repository.jpa.AdapterOperations;
import org.modelmapper.ModelMapper;
import org.reactivecommons.utils.ObjectMapper;
import org.springframework.stereotype.Repository;

@Repository
public class EntityRepositoryAdapter extends AdapterOperations<DomainEntity, DataEntity, String, DataRepository> {

    public EntityRepositoryAdapter(DataRepository repository, ModelMapper mapper) {
        super(repository, mapper, d -> mapper.map(d, DomainEntity.DomainEntityBuilder.class).build());
    }
}
