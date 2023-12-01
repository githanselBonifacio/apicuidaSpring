package co.com.sura.postgres.repository.maestros.repository;

import co.com.sura.postgres.repository.maestros.data.EstadoCitaData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface EstadoCitaRepository extends ReactiveCrudRepository<EstadoCitaData,Integer> {
}
