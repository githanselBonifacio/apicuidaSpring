package co.com.sura.postgres.maestros.repository;

import co.com.sura.postgres.maestros.data.EstadoCitaData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface EstadoCitaRepository extends ReactiveCrudRepository<EstadoCitaData,Integer> {
}
