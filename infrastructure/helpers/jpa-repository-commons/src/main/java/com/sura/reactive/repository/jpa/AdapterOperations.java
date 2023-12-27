package com.sura.reactive.repository.jpa;

import org.modelmapper.ModelMapper;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.lang.reflect.ParameterizedType;
import java.util.function.Function;


public abstract class AdapterOperations<E, D, I, R extends ReactiveCrudRepository<D, I>> {

    protected R repository;
    protected ModelMapper mapper;
    private Class<D> dataClass;
    private Function<D, E> toEntityFn;

    @SuppressWarnings("unchecked")
    protected AdapterOperations(R repository, ModelMapper mapper, Function<D, E> toEntityFn) {
        this.repository = repository;
        this.mapper = mapper;
        ParameterizedType genericSuperclass = (ParameterizedType) this.getClass().getGenericSuperclass();
        this.dataClass = (Class<D>) genericSuperclass.getActualTypeArguments()[1];
        this.toEntityFn = toEntityFn;
    }

    public Mono<E> save(E entity) {
        return Mono.just(entity)
                .map(this::toData)
                .flatMap(this::saveData)
                .thenReturn(entity);
    }

    protected Flux<E> saveAllEntities(Flux<E> entities) {
        return entities.map(this::toData).collectList()
                .flatMapMany(this::saveData).map(this::toEntity);
    }

    private Mono<E> doQuery(Mono<D> query) {
        return query.map(this::toEntity);
    }

    public Mono<E> findById(I id) {
        return repository.findById(id)
                .map(this::toEntity);
    }


    protected Flux<E> doQueryMany(Flux<D> query) {
        return query
                .map(this::toEntity);
    }

    protected D toData(E entity) {
        return mapper.map(entity, dataClass);
    }

    protected E toEntity(D data) {
        return toEntityFn.apply(data);
    }

    protected Mono<D> saveData(D data) {
        return repository.save(data);
    }

    protected Flux<D> saveData(Iterable<D> data) {
        return repository.saveAll(data);
    }

    protected Mono<Void> deleteById(I id) {
        return repository.deleteById(id);
    }
}
