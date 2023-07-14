package co.com.sura.postgres;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.cache.CacheManager;
import org.springframework.data.repository.CrudRepository;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.List;
import java.util.Objects;

public abstract class RepositoryOperations<M, E, PK, R extends CrudRepository<E, PK>> {
    public final R repository;
    //private final ModelMapper mapper;
    private final ObjectMapper objectMapper;
    private final CacheManager cacheManager;
    Class<M> modelClass;
    Class<E> entityClass;

    protected RepositoryOperations(ObjectMapper objectMapper, Class<M> modelClass, Class<E> entityClass, R repository, CacheManager cacheManager) {
        //this.mapper = mapper;
        this.modelClass = modelClass;
        this.entityClass = entityClass;
        this.repository = repository;
        this.objectMapper = objectMapper;
        this.cacheManager = cacheManager;
    }

    protected E toEntity(M model) {
        //return mapper.map(model,entityClass);
        return objectMapper.convertValue(model, entityClass);
    }

    protected M toModel(E entity) {
        //return mapper.map(entity,modelClass);
        return objectMapper.convertValue(entity, modelClass);
    }

    protected Flux<M> doMany(Iterable<E> repositoryResult) {
        return repositoryResult != null ?
                Flux.fromIterable(repositoryResult)
                        .map(this::toModel) :
                Flux.empty();
    }

    protected Mono<M> doOne(E repositoryResult) {
        return repositoryResult != null ?
                Mono.just(repositoryResult)
                        .map(this::toModel) :
                Mono.empty();
    }

    @Transactional
    protected Mono<M> save(M model) {
        clearCache();
        return Mono.just(model)
                .map(this::toEntity)
                .map(repository::save)
                .map(this::toModel);
    }

    protected Flux<M> findAll() {
        return doMany(repository.findAll());
    }

    protected Mono<M> findById(PK pk) {
        return Mono.just(pk)
                .map(repository::findById)
                .flatMap(a -> a.isPresent() ? doOne(a.get()) : Mono.empty());
    }

    @Transactional
    protected Flux<M> save(Flux<M> valueObjectFlux) {
        clearCache();
        return valueObjectFlux
                .map(this::toEntity)
                .collectList()
                .map(repository::saveAll)
                .flatMapMany(Flux::fromIterable)
                .map(this::toModel);
    }

    protected Disposable clearCache() {
        return Flux.fromIterable(cacheManager.getCacheNames())
                .map(cacheName -> {
                    Objects.requireNonNull(cacheManager.getCache(cacheName)).clear();
                    return cacheName;
                })
                .collectList()
                .subscribeOn(Schedulers.boundedElastic())
                .subscribe();
    }
}
