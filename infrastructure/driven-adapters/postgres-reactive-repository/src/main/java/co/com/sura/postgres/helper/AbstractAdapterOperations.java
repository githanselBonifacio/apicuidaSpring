package co.com.sura.postgres.helper;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public abstract class AbstractAdapterOperations<M, E, PK, R extends ReactiveCrudRepository<E, PK>> {

    public R repository;
    private Class<E> entityClass;
    private Class<M> modelClass;
    private ObjectMapper mapper;

    public AbstractAdapterOperations(ObjectMapper objectMapper, Class<M> modelClass, Class<E> entityClass, R repository) {
        this.entityClass = entityClass;
        this.mapper = objectMapper;
        this.modelClass = modelClass;
        this.repository = repository;
    }

    public Mono<M> save(M model) {
        return this.toEntity(model)
                .flatMap(repository::save)
                .flatMap(this::toModel);
    }

    public Flux<M> saveAll(Flux<M> model) {
        return repository.saveAll(model.flatMap(this::toEntity))
                .flatMap(this::toModel);
    }

    public Flux<M> doMany(Flux<E> entity) {
        return entity.flatMap(this::toModel);
    }

    public Mono<M> doOne(Mono<E> entity) {
        return entity.flatMap(this::toModel);
    }

    private Mono<M> toModel(E e) {
        return Mono.just(this.mapper.convertValue(e, this.modelClass));
    }

    private Mono<E> toEntity(M m) {
        return Mono.just(this.mapper.convertValue(m, this.entityClass));
    }

    protected Flux<M> findAll() {
        return doMany(repository.findAll());
    }

    protected Mono<M> findById(PK pk) {
        return repository.findById(pk)
                .flatMap(this::toModel);
    }
}
